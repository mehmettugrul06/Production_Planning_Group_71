# Aggregate planning calculation engine. All quantities and costs are nonnegative.
# Costs: hiring/firing per worker, holding per unit-period, regular/overtime/subcontract per unit.
# Regular cost is charged to regular production (not unused capacity); idle is reported in units.

ap_num <- function(x, default = 0) { x <- suppressWarnings(as.numeric(x)); ifelse(is.finite(x), x, default) }
ap_check <- function(d, k, w0, i0, ie) {
  stopifnot(nrow(d) >= 1, all(is.finite(d$Demand)), all(d$Demand >= 0),
            all(is.finite(d$Days)), all(d$Days > 0), is.finite(k), k > 0,
            w0 >= 0, i0 >= 0, ie >= 0)
}
ap_defaults <- function() list(hire=200, fire=300, hold=2, regular=10, overtime=15,
                                sub=20, idle=0, ot_frac=.25, sub_cap=1e7)
ap_cost <- function(p, costs) {
  data.frame(Item=c('Hiring','Firing','Holding','Regular production','Overtime',
                       'Subcontracting','Idle capacity','TOTAL'),
             Cost=round(c(sum(p$Hired)*costs$hire, sum(p$Fired)*costs$fire,
                          sum(p$Inventory)*costs$hold, sum(p$Regular)*costs$regular,
                          sum(p$Overtime)*costs$overtime, sum(p$Subcontract)*costs$sub,
                          sum(p$Idle)*costs$idle,
                          sum(p$Hired)*costs$hire+sum(p$Fired)*costs$fire+
                            sum(p$Inventory)*costs$hold+sum(p$Regular)*costs$regular+
                            sum(p$Overtime)*costs$overtime+sum(p$Subcontract)*costs$sub+
                            sum(p$Idle)*costs$idle), 2))
}
ap_plan <- function(d,w,r,o,s,k,w0,i0,costs) {
  n <- nrow(d); inv <- numeric(n); h <- f <- numeric(n); idle <- numeric(n)
  prev <- w0; lastinv <- i0
  for(t in seq_len(n)) {
    h[t] <- max(0, w[t]-prev); f[t] <- max(0, prev-w[t]); prev <- w[t]
    inv[t] <- lastinv + r[t]+o[t]+s[t]-d$Demand[t]
    if(inv[t] < -1e-5) stop('Infeasible plan: demand is not met.')
    inv[t] <- max(0, inv[t]); lastinv <- inv[t]
    idle[t] <- max(0, w[t]*k*d$Days[t]-r[t])
  }
  out <- data.frame(Period=d$Period, Demand=d$Demand, Days=d$Days,
      Workers=round(w,5), Hired=round(h,5), Fired=round(f,5),
      Regular=round(r,5), Overtime=round(o,5), Subcontract=round(s,5),
      Production=round(r+o+s,5), Inventory=round(inv,5), Idle=round(idle,5),
      check.names=FALSE)
  out
}
# Production policy: produce only what is necessary, with a backward feasibility
# requirement to attain a specified final inventory; use subcontract as emergency overflow.
ap_simulate <- function(d, w, k, w0, i0, ie, costs, fixed_regular=NULL) {
  n <- nrow(d); r <- o <- s <- numeric(n); inv <- i0
  total <- sum(d$Demand)+ie-i0
  if(total < -1e-8) stop('Initial inventory exceeds all demand plus ending inventory; disposal is not modeled.')
  for(t in seq_len(n)) {
    cap <- w[t]*k*d$Days[t]
    req <- max(0, d$Demand[t]-inv + if(t==n) ie else 0)
    if(is.null(fixed_regular)) r[t] <- min(cap,req) else r[t] <- min(cap,fixed_regular[t])
    need <- max(0, req-r[t]); o[t] <- min(need, cap*costs$ot_frac); need <- need-o[t]
    s[t] <- need
    if(s[t] > costs$sub_cap + 1e-7) stop('Subcontracting limit makes the plan infeasible.')
    inv <- inv + r[t]+o[t]+s[t]-d$Demand[t]
    if(inv < -1e-6) stop('Demand cannot be met.')
  }
  ap_plan(d,w,r,o,s,k,w0,i0,costs)
}
ap_blocks <- function(n, starts) {
  starts <- sort(unique(c(1L, as.integer(starts))))
  starts <- starts[starts>=1 & starts<=n]
  split(seq_len(n), findInterval(seq_len(n), starts))
}
ap_heuristic <- function(method,d,k,w0,i0,ie,costs,change_periods=integer(),integer_workers=FALSE) {
  n <- nrow(d); target <- d$Demand
  target[1] <- target[1]-i0; target[n] <- target[n]+ie
  if(method=='Chase') {
    w <- numeric(n); available <- i0
    for(t in seq_len(n)) {
      required <- d$Demand[t] + if(t==n) ie else 0
      w[t] <- max(0,(required-available)/(k*d$Days[t]))
      available <- available + w[t]*k*d$Days[t] - d$Demand[t]
    }
  } else if(method=='Level') {
    # Constant workforce, able to satisfy every prefix without backorders.
    pref <- cumsum(d$Demand); need <- pref - i0
    need[n] <- need[n]+ie
    w <- rep(max(0,max(need/(k*cumsum(d$Days)))),n)
  } else if(method=='Mixed') {
    blocks <- ap_blocks(n,change_periods)
    w <- numeric(n)
    for(b in blocks) {
      demand <- sum(d$Demand[b]) + if(max(b)==n) ie else 0
      if(min(b)==1) demand <- demand-i0
      # Each block targets zero inventory at its end.
      w[b] <- max(0,demand/(k*sum(d$Days[b])))
    }
  } else stop('Unknown heuristic.')
  if(integer_workers) w <- ceiling(w-1e-9)
  # For Level/Mixed, use regular capacity to build stock for late demand.
  if(method=='Chase') return(ap_simulate(d,w,k,w0,i0,ie,costs))
  cap <- w*k*d$Days; r <- o <- s <- numeric(n); inv <- i0
  for(t in seq_len(n)) {
    # Remaining demand that still needs production, including terminal stock.
    needed_total <- max(0, sum(d$Demand[t:nrow(d)])+ie-inv)
    r[t] <- min(cap[t],needed_total)
    short <- max(0,d$Demand[t] + if(t==n) ie else 0 - inv - r[t])
    o[t] <- min(short,cap[t]*costs$ot_frac); s[t] <- short-o[t]
    if(s[t] > costs$sub_cap+1e-7) stop('Subcontracting limit makes the plan infeasible.')
    inv <- inv+r[t]+o[t]+s[t]-d$Demand[t]
  }
  ap_plan(d,w,r,o,s,k,w0,i0,costs)
}
# LP and MILP use lpSolve. Continuous workforce plan, or a feasible plan with
# the fractional solution's workforce fixed at the rounded-up values.
ap_lp <- function(d,k,w0,i0,ie,costs,allow_ot=TRUE,allow_idle=TRUE,allow_sub=TRUE,
                  fixed_w=NULL) {
  if(!requireNamespace('lpSolve',quietly=TRUE)) stop('Install package lpSolve.')
  n <- nrow(d); names <- c('W','H','F','R','O','S','I','Idle')
  id <- function(x,t) match(x,names)*n-n+t
  nv <- length(names)*n; obj <- numeric(nv)
  for(t in seq_len(n)) {
    for(z in c('H','F','R','O','S','I','Idle')) {
      v <- c(H=costs$hire,F=costs$fire,R=costs$regular,O=costs$overtime,S=costs$sub,I=costs$hold,Idle=costs$idle)[z]
      obj[id(z,t)] <- unname(v)
    }
  }
  A <- list(); dirs <- character(); rhs <- numeric()
  add <- function(indices,values,dir,right) {
    v <- numeric(nv); v[indices] <- values
    A[[length(A)+1L]] <<- v; dirs <<- c(dirs,dir); rhs <<- c(rhs,right)
  }
  for(t in seq_len(n)) {
    # W_t - H_t + F_t - W_(t-1) = 0
    inds <- c(id('W',t),id('H',t),id('F',t)); vals <- c(1,-1,1)
    if(t>1) { inds<-c(inds,id('W',t-1));vals<-c(vals,-1) }
    add(inds,vals,'=',if(t==1) w0 else 0)
    # R + O + S + I_(t-1) - I_t = demand - (initial inventory for t1)
    inds <- c(id('R',t),id('O',t),id('S',t),id('I',t)); vals <- c(1,1,1,-1)
    if(t>1) {inds<-c(inds,id('I',t-1));vals<-c(vals,1)}
    add(inds,vals,'=',d$Demand[t]-if(t==1) i0 else 0)
    capacity <- k*d$Days[t]
    # R + Idle = available regular capacity (unless idle disabled, then R=capacity)
    add(c(id('R',t),id('Idle',t),id('W',t)), c(1,1,-capacity),'=',0)
    if(!allow_idle) add(id('Idle',t),1,'=',0)
    add(c(id('O',t),id('W',t)),c(1,-capacity*costs$ot_frac),'<=',0)
    if(!allow_ot) add(id('O',t),1,'=',0)
    add(id('S',t),1,'<=',if(allow_sub) costs$sub_cap else 0)
    if(!is.null(fixed_w)) add(id('W',t),1,'=',fixed_w[t])
  }
  add(id('I',n),1,'=',ie)
  m <- do.call(rbind,A)
  sol <- lpSolve::lp('min',obj,m,dirs,rhs)
  if(sol$status!=0) stop(paste('LP solver returned status',sol$status,'(infeasible or numerical issue).'))
  x <- sol$solution
  get <- function(z) pmax(0,x[sapply(seq_len(n),function(t) id(z,t))])
  ap_plan(d,get('W'),get('R'),get('O'),get('S'),k,w0,i0,costs)
}
# Transportation supplies: initial inventory, regular, OT and subcontract in each period.
# All arcs go forward in time only. The final demand column represents ending inventory.
ap_transport <- function(d,k,w0,i0,ie,costs,workers,method='Optimal LP') {
  if(!requireNamespace('lpSolve',quietly=TRUE)) stop('Install package lpSolve.')
  n<-nrow(d)
  sources <- data.frame(Source='Initial inventory', From=1L, Type='Initial',Supply=i0,UnitCost=0)
  for(t in seq_len(n)) {
    cap <- workers[t]*k*d$Days[t]
    sources<-rbind(sources,
      data.frame(Source=paste0(d$Period[t],' - Regular'),From=t,Type='Regular',Supply=cap,UnitCost=costs$regular),
      data.frame(Source=paste0(d$Period[t],' - Overtime'),From=t,Type='Overtime',Supply=cap*costs$ot_frac,UnitCost=costs$overtime),
      data.frame(Source=paste0(d$Period[t],' - Subcontract'),From=t,Type='Subcontract',Supply=costs$sub_cap,UnitCost=costs$sub))
  }
  demands <- c(d$Demand,ie)
  labels <- c(as.character(d$Period),'Ending inventory')
  arc <- expand.grid(a=seq_len(nrow(sources)),b=seq_along(demands))
  arc <- arc[arc$b>=sources$From[arc$a],,drop=FALSE]
  arc <- arc[!(arc$b==n+1 & sources$From[arc$a]>n),,drop=FALSE]
  arc$cost <- sources$UnitCost[arc$a]+costs$hold*pmax(0,ifelse(arc$b==n+1,n+1,arc$b)-sources$From[arc$a])
  if(nrow(arc)<1) stop('No feasible transportation routes.')
  x <- numeric(nrow(arc))
  if(method=='Optimal LP') {
    A <- matrix(0,nrow(sources)+length(demands),nrow(arc))
    for(j in seq_len(nrow(arc))) { A[arc$a[j],j]<-1;A[nrow(sources)+arc$b[j],j]<-1 }
    sol<-lpSolve::lp('min',arc$cost,A,
         c('=',rep('<=',nrow(sources)-1),rep('=',length(demands))),c(sources$Supply,demands))
    if(sol$status!=0) stop('Transportation LP is infeasible; increase source capacities.')
    x<-pmax(0,sol$solution)
  } else {
    remS <- sources$Supply;remD<-demands
    order_arcs <- if(method=='North-West Corner') seq_len(nrow(arc)) else order(arc$cost,arc$a,arc$b)
    # NW order must be demand-first, source-second (sources are chronological).
    if(method=='North-West Corner') order_arcs<-order(arc$b,arc$a)
    for(j in order_arcs) {
      v <- min(remS[arc$a[j]],remD[arc$b[j]])
      if(v>0) {x[j]<-v;remS[arc$a[j]]<-remS[arc$a[j]]-v;remD[arc$b[j]]<-remD[arc$b[j]]-v}
    }
    if(any(remD>1e-5) || remS[1]>1e-5) stop(paste(method,'greedy allocation is infeasible. Use Optimal LP.'))
  }
  shipments<-data.frame(Source=sources$Source[arc$a],Destination=labels[arc$b],
                         Units=round(x,5),UnitCost=round(arc$cost,4),Cost=round(x*arc$cost,2))
  shipments <- shipments[x>1e-6,,drop=FALSE]
  r <- o <- s <- numeric(n)
  for(t in seq_len(n)) {
    for(typ in c('Regular','Overtime','Subcontract')) {
      qty<-sum(x[arc$a==which(sources$From==t & sources$Type==typ)])
      if(typ=='Regular') r[t]<-qty
      if(typ=='Overtime') o[t]<-qty
      if(typ=='Subcontract') s[t]<-qty
    }
  }
  # Initial stock source may go to final inventory. Inventory trajectory follows flow.
  out <- ap_plan(d,workers,r,o,s,k,w0,i0,costs)
  list(plan=out,shipments=shipments,transport_cost=round(sum(x*arc$cost),2))
}
# Square-root disaggregation (user-supplied nonnegative family weights).
ap_disaggregate <- function(p,families) {
  if(!all(c('Family','Weight') %in% names(families))) stop('Family and Weight are required.')
  if(anyNA(families$Weight) || any(families$Weight<0) || sum(families$Weight)<=0)
    stop('Weights must be nonnegative with a positive sum.')
  share <- sqrt(families$Weight)/sum(sqrt(families$Weight))
  out<-do.call(rbind,lapply(seq_len(nrow(p)),function(t)
    data.frame(Period=p$Period[t],Family=families$Family,Share=round(share,5),
     Demand=round(p$Demand[t]*share,5),Production=round(p$Production[t]*share,5),
     Inventory=round(p$Inventory[t]*share,5))))
  rownames(out)<-NULL; out
}
