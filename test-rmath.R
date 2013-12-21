context( "R math" )
sourceCpp( "cpp/rmath.cpp", env = environment()

test_that( "(dpq)norm gives correct results", {
    x <- 0.25
    a <- 1.25
    b <- 2.50
    expect_equal(runit_dnorm(x, a, b),
                c(dnorm(x, a, b, log=FALSE), dnorm(x, a, b, log=TRUE))
                )

    expect_equal(runit_pnorm(x, a, b),
                c(pnorm(x, a, b, lower=TRUE, log=FALSE),  pnorm(log(x), a, b, lower=TRUE, log=TRUE),
                  pnorm(x, a, b, lower=FALSE, log=FALSE), pnorm(log(x), a, b, lower=FALSE, log=TRUE))
                )

    expect_equal(runit_qnorm(x, a, b),
                c(qnorm(x, a, b, lower=TRUE, log=FALSE),  qnorm(log(x), a, b, lower=TRUE,  log=TRUE),
                  qnorm(x, a, b, lower=FALSE, log=FALSE), qnorm(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    v <- seq(0.0, 1.0, by=0.1)
    expect_equal(runit_dnorm_2(v),
                list( false_noMean_noSd = dnorm(v),
                      false_noSd = dnorm(v, 0.0),
                      false = dnorm(v, 0.0, 1.0),
                      true = dnorm(v, 0.0, 1.0, log=TRUE ),
                      true_noSd = dnorm(v, 0.0, log=TRUE ),
                      true_noMean_noSd = dnorm(v, log=TRUE )
                ))
    v <- qnorm(seq(0.0, 1.0, by=0.1))
    expect_equal(runit_pnorm_2(v),
                list(lowerNoLog = pnorm(v),
                     lowerLog   = pnorm(v, log=TRUE ),
                     upperNoLog = pnorm(v, lower=FALSE),
                     upperLog   = pnorm(v, lower=FALSE, log=TRUE)
                     )
                     )
    ## Borrowed from R's d-p-q-r-tests.R
    z <- c(-Inf,Inf,NA,NaN, rt(1000, df=2))
    z.ok <- z > -37.5 | !is.finite(z)
    pz <- runit_pnorm_2(z)
    expect_equal(pz$lowerNoLog, 1 - pz$upperNoLog)
    expect_equal(pz$lowerNoLog, runit_pnorm_2(-z)$upperNoLog)
    expect_equal(log(pz$lowerNoLog[z.ok]), pz$lowerLog[z.ok])
    ## FIXME: Add tests that use non-default mu and sigma

    expect_equal(runit_qnorm_prob(c(0, 1, 1.1, -.1)),
                list(lower = c(-Inf, Inf, NaN, NaN),
                     upper = c(Inf, -Inf, NaN, NaN)
                     )
                     )
    ## Borrowed from R's d-p-q-r-tests.R and Wichura (1988)
    expect_equal(runit_qnorm_prob(c( 0.25,  .001,	 1e-20))$lower,
                       c(-0.6744897501960817, -3.090232306167814, -9.262340089798408),
                       tol = 1e-15)

    expect_equal(runit_qnorm_log(c(-Inf, 0, 0.1)),
                list(lower = c(-Inf, Inf, NaN),
                     upper = c(Inf, -Inf, NaN)
                     )
                     )
    expect_equal(runit_qnorm_log(-1e5)$lower, -447.1974945)
})   

test_that( "(dpq)unif is correct", {
    x <- 0.25
    a <- 1.25
    b <- 2.50
    expect_equal(runit_dunif(x, a, b),
                c(dunif(x, a, b, log=FALSE), dunif(x, a, b, log=TRUE)))

    expect_equal(runit_punif(x, a, b),
                c(punif(x, a, b, lower=TRUE, log=FALSE),  punif(log(x), a, b, lower=TRUE, log=TRUE),
                  punif(x, a, b, lower=FALSE, log=FALSE), punif(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    expect_equal(runit_qunif(x, a, b),
                c(qunif(x, a, b, lower=TRUE, log=FALSE),  qunif(log(x), a, b, lower=TRUE,  log=TRUE),
                  qunif(x, a, b, lower=FALSE, log=FALSE), qunif(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    vv <- seq(0, 1, by = 0.1)
    expect_equal(runit_dunif_2(vv),
                list(
                    NoLog_noMin_noMax = dunif(vv),
                    NoLog_noMax = dunif(vv, 0),
                    NoLog = dunif(vv, 0, 1),
                     Log   = dunif(vv, 0, 1, log=TRUE),
                     Log_noMax   = dunif(vv, 0, log=TRUE)
                     #,Log_noMin_noMax   = dunif(vv, log=TRUE)  ## wrong answer
                     ))
    v <- qunif(seq(0.0, 1.0, by=0.1))
    expect_equal(runit_punif_2(v),
                list(lowerNoLog = punif(v),
                     lowerLog   = punif(v, log=TRUE ),
                     upperNoLog = punif(v, lower=FALSE),
                     upperLog   = punif(v, lower=FALSE, log=TRUE)
                     )
                     )
    # TODO: also borrow from R's d-p-q-r-tests.R

    expect_equal(runit_qunif_prob(c(0, 1, 1.1, -.1)),
                list(lower = c(0, 1, NaN, NaN),
                     upper = c(1, 0, NaN, NaN)
                     )
                     )
    # TODO: also borrow from R's d-p-q-r-tests.R
})

test_that( "(dpq)gamma is correct", {
    x <- 0.25
    a <- 1.0
    b <- 1.0
    expect_equal(runit_dgamma(x, a, b),
                c(dgamma(x, a, b, log=FALSE), dgamma(x, a, b, log=TRUE))
                )

    expect_equal(runit_pgamma(x, a, b),
                c(pgamma(x, a, b, lower=TRUE, log=FALSE),  pgamma(log(x), a, b, lower=TRUE, log=TRUE),
                  pgamma(x, a, b, lower=FALSE, log=FALSE), pgamma(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    expect_equal(runit_qgamma(x, a, b),
                c(qgamma(x, a, b, lower=TRUE, log=FALSE),  qgamma(log(x), a, b, lower=TRUE,  log=TRUE),
                  qgamma(x, a, b, lower=FALSE, log=FALSE), qgamma(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    v <- 1:4
    expect_equal(runit_dgamma_2(v),
                list( NoLog = dgamma(v, 1.0, 1.0),
                      Log = dgamma(v, 1.0, 1.0, log = TRUE ),
                      Log_noRate = dgamma(v, 1.0, log = TRUE )
                ))

    v <- (1:9)/10
    expect_equal(runit_pgamma_2(v),
                list(lowerNoLog = pgamma(v, shape = 2.0),
                     lowerLog   = pgamma(v, shape = 2.0, log=TRUE ),
                     upperNoLog = pgamma(v, shape = 2.0, lower=FALSE),
                     upperLog   = pgamma(v, shape = 2.0, lower=FALSE, log=TRUE)
                     )
                     )

})

test_that( "(dpq)beta is correct", {
    x <- 0.25
    a <- 0.8
    b <- 2.5
    expect_equal(runit_dbeta(x, a, b),
                c(dbeta(x, a, b, log=FALSE), dbeta(x, a, b, log=TRUE))
                )

    expect_equal(runit_pbeta(x, a, b),
                c(pbeta(x, a, b, lower=TRUE, log=FALSE),  pbeta(log(x), a, b, lower=TRUE, log=TRUE),
                  pbeta(x, a, b, lower=FALSE, log=FALSE), pbeta(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    expect_equal(runit_qbeta(x, a, b),
                c(qbeta(x, a, b, lower=TRUE, log=FALSE),  qbeta(log(x), a, b, lower=TRUE,  log=TRUE),
                  qbeta(x, a, b, lower=FALSE, log=FALSE), qbeta(log(x), a, b, lower=FALSE, log=TRUE))
                  )
                  
    vv <- seq(0, 1, by = 0.1)
    a <- 0.5; b <- 2.5
    expect_equal(runit_dbeta_2(vv, a, b),
                list(
                     NoLog = dbeta(vv, a, b),
                     Log   = dbeta(vv, a, b, log=TRUE)
                     )
                     )                  

    a <- 0.5; b <- 2.5
    v <- qbeta(seq(0.0, 1.0, by=0.1), a, b)
    expect_equal(runit_pbeta_2(v, a, b),
                list(lowerNoLog = pbeta(v, a, b),
                     lowerLog   = pbeta(v, a, b,              log=TRUE),
                     upperNoLog = pbeta(v, a, b, lower=FALSE),
                     upperLog   = pbeta(v, a, b, lower=FALSE, log=TRUE)
                     )
                     )
    ## Borrowed from R's d-p-q-r-tests.R
    x <- c(.01, .10, .25, .40, .55, .71, .98)
    pbval <- c(-0.04605755624088, -0.3182809860569, -0.7503593555585,
               -1.241555830932, -1.851527837938, -2.76044482378, -8.149862739881)
    expect_equal(runit_pbeta_2(x, 0.8, 2)$upperLog, pbval)
    expect_equal(runit_pbeta_2(1-x, 2, 0.8)$lowerLog, pbval)

})

test_that( "(dpq)lnorm is correct", {
    x <- 0.25
    a <- 0.8
    b <- 2.5
    expect_equal(runit_dlnorm(x, a, b),
                c(dlnorm(x, a, b, log=FALSE), dlnorm(x, a, b, log=TRUE))
                )

    expect_equal(runit_plnorm(x, a, b),
                c(plnorm(x, a, b, lower=TRUE, log=FALSE),  plnorm(log(x), a, b, lower=TRUE, log=TRUE),
                  plnorm(x, a, b, lower=FALSE, log=FALSE), plnorm(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    expect_equal(runit_qlnorm(x, a, b),
                c(qlnorm(x, a, b, lower=TRUE, log=FALSE),  qlnorm(log(x), a, b, lower=TRUE,  log=TRUE),
                  qlnorm(x, a, b, lower=FALSE, log=FALSE), qlnorm(log(x), a, b, lower=FALSE, log=TRUE))
                  )
})

test_that( "(dpq)chisq is correct", { 
    x <- 0.25
    a <- 0.8
    expect_equal(runit_dchisq(x, a),
                c(dchisq(x, a, log=FALSE), dchisq(x, a, log=TRUE))
                )

    expect_equal(runit_pchisq(x, a),
                c(pchisq(x, a, lower=TRUE, log=FALSE),  pchisq(log(x), a, lower=TRUE, log=TRUE),
                  pchisq(x, a, lower=FALSE, log=FALSE), pchisq(log(x), a, lower=FALSE, log=TRUE))
                  )

    expect_equal(runit_qchisq(x, a),
                c(qchisq(x, a, lower=TRUE, log=FALSE),  qchisq(log(x), a, lower=TRUE,  log=TRUE),
                  qchisq(x, a, lower=FALSE, log=FALSE), qchisq(log(x), a, lower=FALSE, log=TRUE))
                  )

    v <- (1:9)/10
    expect_equal(runit_pchisq_2(v),
                list(lowerNoLog = pchisq(v, 6, lower=TRUE, log=FALSE),
                     lowerLog   = pchisq(v, 6, log=TRUE ),
                     upperNoLog = pchisq(v, 6, lower=FALSE),
                     upperLog   = pchisq(v, 6, lower=FALSE, log=TRUE)
                     )
                     )

})

test_that( "(dpq)nchisq is correct", {
    x <- 0.25
    a <- 0.8
    b <- 2.5
    expect_equal(runit_dnchisq(x, a, b),
                c(dchisq(x, a, b, log=FALSE), dchisq(x, a, b, log=TRUE))
                )

    expect_equal(runit_pnchisq(x, a, b),
                c(pchisq(x, a, b, lower=TRUE, log=FALSE),  pchisq(log(x), a, b, lower=TRUE, log=TRUE),
                  pchisq(x, a, b, lower=FALSE, log=FALSE), pchisq(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    expect_equal(runit_qnchisq(x, a, b),
                c(qchisq(x, a, b, lower=TRUE, log=FALSE),  qchisq(log(x), a, b, lower=TRUE,  log=TRUE),
                  qchisq(x, a, b, lower=FALSE, log=FALSE), qchisq(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    v <- (1:9)/10
    expect_equal(runit_pnchisq_2(v),
                list(lowerNoLog = pchisq(v, 6, ncp=2.5, lower=TRUE, log=FALSE),
                     lowerLog   = pchisq(v, 6, ncp=2.5, log=TRUE ),
                     upperNoLog = pchisq(v, 6, ncp=2.5, lower=FALSE),
                     upperLog   = pchisq(v, 6, ncp=2.5, lower=FALSE, log=TRUE)
                     )
                     )
})

test_that( "(dpq)f is correct", {
    x <- 0.25
    a <- 0.8
    b <- 2.5
    expect_equal(runit_df(x, a, b),
                c(df(x, a, b, log=FALSE), df(x, a, b, log=TRUE))
                )

    expect_equal(runit_pf(x, a, b),
                c(pf(x, a, b, lower=TRUE, log=FALSE),  pf(log(x), a, b, lower=TRUE, log=TRUE),
                  pf(x, a, b, lower=FALSE, log=FALSE), pf(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    expect_equal(runit_qf(x, a, b),
                c(qf(x, a, b, lower=TRUE, log=FALSE),  qf(log(x), a, b, lower=TRUE,  log=TRUE),
                  qf(x, a, b, lower=FALSE, log=FALSE), qf(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    v <- (1:9)/10
    expect_equal(runit_pf_2(v),
                list(lowerNoLog = pf(v, 6, 8, lower=TRUE, log=FALSE),
                     lowerLog   = pf(v, 6, 8, log=TRUE ),
                     upperNoLog = pf(v, 6, 8, lower=FALSE),
                     upperLog   = pf(v, 6, 8, lower=FALSE, log=TRUE)
                     )
                     )
})

test_that( "(dpq)t is correct", {
    x <- 0.25
    a <- 0.8
    expect_equal(runit_dt(x, a),
                c(dt(x, a, log=FALSE), dt(x, a, log=TRUE))
                )

    expect_equal(runit_pt(x, a),
                c(pt(x, a, lower=TRUE, log=FALSE),  pt(log(x), a, lower=TRUE, log=TRUE),
                  pt(x, a, lower=FALSE, log=FALSE), pt(log(x), a, lower=FALSE, log=TRUE))
                  )

    expect_equal(runit_qt(x, a),
                c(qt(x, a, lower=TRUE, log=FALSE),  qt(log(x), a, lower=TRUE,  log=TRUE),
                  qt(x, a, lower=FALSE, log=FALSE), qt(log(x), a, lower=FALSE, log=TRUE))
                  )

	v <- seq(0.0, 1.0, by=0.1)
    expect_equal(runit_dt_2(v),
                list( false = dt(v, 5),
                      true = dt(v, 5, log=TRUE ) # NB: need log=TRUE here
                ))
	v <- seq(0.0, 1.0, by=0.1)
    expect_equal(runit_pt_2(v),
                list(lowerNoLog = pt(v, 5),
                     lowerLog   = pt(v, 5,              log=TRUE),
                     upperNoLog = pt(v, 5, lower=FALSE),
                     upperLog   = pt(v, 5, lower=FALSE, log=TRUE) )
                     )
    v <- seq(0.05, 0.95, by=0.05)
    ( x1 <- runit_qt_2(v, 5, FALSE, FALSE) )
    ( x2 <- qt(v, df=5, lower=FALSE, log=FALSE) )
    expect_equal(x1, x2)

    ( x1 <- runit_qt_2(v, 5, TRUE, FALSE) )
    ( x2 <- qt(v, df=5, lower=TRUE, log=FALSE) )
    expect_equal(x1, x2)

	  ( x1 <- runit_qt_2(-v, 5, FALSE, TRUE) )
    ( x2 <- qt(-v, df=5, lower=FALSE, log=TRUE) )
    expect_equal(x1, x2)

    ( x1 <- runit_qt_2(-v, 5, TRUE, TRUE) )
    ( x2 <- qt(-v, df=5, lower=TRUE, log=TRUE) )
    expect_equal(x1, x2)

})

test_that( "(dpq)binom is correct", {
    x <- 5
    a <- 10
    b <- 0.5
    expect_equal(runit_dbinom(x, a, b),
                c(dbinom(x, a, b, log=FALSE), dbinom(x, a, b, log=TRUE))
                )

    expect_equal(runit_pbinom(x, a, b),
                c(pbinom(x, a, b, lower=TRUE, log=FALSE),  pbinom(log(x), a, b, lower=TRUE, log=TRUE),
                  pbinom(x, a, b, lower=FALSE, log=FALSE), pbinom(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    x <- x/a
    expect_equal(runit_qbinom(x, a, b),
                c(qbinom(x, a, b, lower=TRUE, log=FALSE),  qbinom(log(x), a, b, lower=TRUE,  log=TRUE),
                  qbinom(x, a, b, lower=FALSE, log=FALSE), qbinom(log(x), a, b, lower=FALSE, log=TRUE))
                  )

	v <- 1:10
	expect_equal(runit_dbinom_2(v) ,
                list(
                    false = dbinom(v, 10, .5),
                    true = dbinom(v, 10, .5, TRUE )
                )
                )

    n <- 20
    p <- 0.5
    vv <- 0:n
    expect_equal(runit_pbinom_2(vv, n, p),
                list(lowerNoLog = pbinom(vv, n, p),
                     lowerLog   = pbinom(vv, n, p, log=TRUE),
                     upperNoLog = pbinom(vv, n, p, lower=FALSE),
                     upperLog   = pbinom(vv, n, p, lower=FALSE, log=TRUE)
                     )
                     )


    n <- 20
    p <- 0.5
    vv <- seq(0, 1, by = 0.1)
    expect_equal(runit_qbinom_prob(vv, n, p),
                list(lower = qbinom(vv, n, p),
                     upper = qbinom(vv, n, p, lower=FALSE)
                     )
                     )
})

test_that( "(dpq)cauchy works", {
    x <- 0.25
    a <- 0.8
    b <- 2.5
    expect_equal(runit_dcauchy(x, a, b),
                c(dcauchy(x, a, b, log=FALSE), dcauchy(x, a, b, log=TRUE))
                )

    expect_equal(runit_pcauchy(x, a, b),
                c(pcauchy(x, a, b, lower=TRUE, log=FALSE),  pcauchy(log(x), a, b, lower=TRUE, log=TRUE),
                  pcauchy(x, a, b, lower=FALSE, log=FALSE), pcauchy(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    expect_equal(runit_qcauchy(x, a, b),
                c(qcauchy(x, a, b, lower=TRUE, log=FALSE),  qcauchy(log(x), a, b, lower=TRUE,  log=TRUE),
                  qcauchy(x, a, b, lower=FALSE, log=FALSE), qcauchy(log(x), a, b, lower=FALSE, log=TRUE))
                  )
    location <- 0.5
    scale <- 1.5
    vv <- 1:5
    expect_equal(runit_pcauchy_2(vv, location, scale),
                list(lowerNoLog = pcauchy(vv, location, scale),
                     lowerLog   = pcauchy(vv, location, scale, log=TRUE),
                     upperNoLog = pcauchy(vv, location, scale, lower=FALSE),
                     upperLog   = pcauchy(vv, location, scale, lower=FALSE, log=TRUE)
                     )
                     )

})

test_that( "(dpq)exp works", {
    x <- 0.25
    a <- 1.0
    expect_equal(runit_dexp(x, a),
                c(dexp(x, a, log=FALSE), dexp(x, a, log=TRUE))
                )

    expect_equal(runit_pexp(x, a),
                c(pexp(x, a, lower=TRUE, log=FALSE),  pexp(log(x), a, lower=TRUE, log=TRUE),
                  pexp(x, a, lower=FALSE, log=FALSE), pexp(log(x), a, lower=FALSE, log=TRUE))
                  )

    expect_equal(runit_qexp(x, a),
                c(qexp(x, a, lower=TRUE, log=FALSE),  qexp(log(x), a, lower=TRUE,  log=TRUE),
                  qexp(x, a, lower=FALSE, log=FALSE), qexp(log(x), a, lower=FALSE, log=TRUE))
                  )
})

test_that( "(dpq)geom works", {
    x <- 1
    a <- 0.75
    expect_equal(runit_dgeom(x, a),
                c(dgeom(x, a, log=FALSE), dgeom(x, a, log=TRUE))
                )

    expect_equal(runit_pgeom(x, a),
                c(pgeom(x, a, lower=TRUE, log=FALSE),  pgeom(log(x), a, lower=TRUE, log=TRUE),
                  pgeom(x, a, lower=FALSE, log=FALSE), pgeom(log(x), a, lower=FALSE, log=TRUE))
                  )

    expect_equal(runit_qgeom(x, a),
                c(qgeom(x, a, lower=TRUE, log=FALSE),  qgeom(log(x), a, lower=TRUE,  log=TRUE),
                  qgeom(x, a, lower=FALSE, log=FALSE), qgeom(log(x), a, lower=FALSE, log=TRUE))
                  )
})

test_that( "(dpq)hyper works", {
    x <- 5
    a <- 10
    b <- 7
    c <- 8
    expect_equal(runit_dhyper(x, a, b, c),
                c(dhyper(x, a, b, c, log=FALSE), dhyper(x, a, b, c, log=TRUE))
                )

    expect_equal(runit_phyper(x, a, b, c),
                c(phyper(x, a, b, c, lower=TRUE, log=FALSE),  phyper(log(x), a, b, c, lower=TRUE, log=TRUE),
                  phyper(x, a, b, c, lower=FALSE, log=FALSE), phyper(log(x), a, b, c, lower=FALSE, log=TRUE))
                  )

    x <- x/a
    expect_equal(runit_qhyper(x, a, b, c),
                c(qhyper(x, a, b, c, lower=TRUE, log=FALSE),  qhyper(log(x), a, b, c, lower=TRUE,  log=TRUE),
                  qhyper(x, a, b, c, lower=FALSE, log=FALSE), qhyper(log(x), a, b, c, lower=FALSE, log=TRUE))
                  )
})

test_that( "(dpq)nbinom works", {
    x <- 2
    a <- 8
    b <- 0.25
    expect_equal(runit_dnbinom(x, a, b),
                c(dnbinom(x, a, b, log=FALSE), dnbinom(x, a, b, log=TRUE))
                )

    expect_equal(runit_pnbinom(x, a, b),
                c(pnbinom(x, a, b, lower=TRUE, log=FALSE),  pnbinom(log(x), a, b, lower=TRUE, log=TRUE),
                  pnbinom(x, a, b, lower=FALSE, log=FALSE), pnbinom(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    x <- x/a
    expect_equal(runit_qnbinom(x, a, b),
                c(qnbinom(x, a, b, lower=TRUE, log=FALSE),  qnbinom(log(x), a, b, lower=TRUE,  log=TRUE),
                  qnbinom(x, a, b, lower=FALSE, log=FALSE), qnbinom(log(x), a, b, lower=FALSE, log=TRUE))
                  )
})

test_that( "(dpq)pois works", {
    x <- 2
    a <- 1.0
    expect_equal(runit_dpois(x, a),
                c(dpois(x, a, log=FALSE), dpois(x, a, log=TRUE))
                )

    expect_equal(runit_ppois(x, a),
                c(ppois(x, a, lower=TRUE, log=FALSE),  ppois(log(x), a, lower=TRUE, log=TRUE),
                  ppois(x, a, lower=FALSE, log=FALSE), ppois(log(x), a, lower=FALSE, log=TRUE))
                  )

    x <- 1/x
    expect_equal(runit_qpois(x, a),
                c(qpois(x, a, lower=TRUE, log=FALSE),  qpois(log(x), a, lower=TRUE,  log=TRUE),
                  qpois(x, a, lower=FALSE, log=FALSE), qpois(log(x), a, lower=FALSE, log=TRUE))
                  )

	v <- 0:5
	expect_equal(runit_dpois_2(v) ,
                list( false = dpois(v, .5),
                      true = dpois(v, .5, TRUE )
                ))
    vv <- 0:20
    expect_equal(runit_ppois_2(vv),
                list(lowerNoLog = ppois(vv, 0.5),
                     lowerLog   = ppois(vv, 0.5,              log=TRUE),
                     upperNoLog = ppois(vv, 0.5, lower=FALSE),
                     upperLog   = ppois(vv, 0.5, lower=FALSE, log=TRUE)
                     )
                     )

    vv <- seq(0, 1, by = 0.1)
    expect_equal(runit_qpois_prob(vv),
                list(lower = qpois(vv, 0.5),
                     upper = qpois(vv, 0.5, lower=FALSE)
                     )
                     )
})

test_that( "(dpq)weibull works", {
    x <- 2
    a <- 8
    b <- 0.25
    expect_equal(runit_dweibull(x, a, b),
                c(dweibull(x, a, b, log=FALSE), dweibull(x, a, b, log=TRUE))
                )

    expect_equal(runit_pweibull(x, a, b),
                c(pweibull(x, a, b, lower=TRUE, log=FALSE),  pweibull(log(x), a, b, lower=TRUE, log=TRUE),
                  pweibull(x, a, b, lower=FALSE, log=FALSE), pweibull(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    x <- x/a
    expect_equal(runit_qweibull(x, a, b),
                c(qweibull(x, a, b, lower=TRUE, log=FALSE),  qweibull(log(x), a, b, lower=TRUE,  log=TRUE),
                  qweibull(x, a, b, lower=FALSE, log=FALSE), qweibull(log(x), a, b, lower=FALSE, log=TRUE))
                  )
} )

test_that( "(dpq)logis works", {
    x <- 2
    a <- 8
    b <- 0.25
    expect_equal(runit_dlogis(x, a, b),
                c(dlogis(x, a, b, log=FALSE), dlogis(x, a, b, log=TRUE))
                )

    expect_equal(runit_plogis(x, a, b),
                c(plogis(x, a, b, lower=TRUE, log=FALSE),  plogis(log(x), a, b, lower=TRUE, log=TRUE),
                  plogis(x, a, b, lower=FALSE, log=FALSE), plogis(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    x <- x/a
    expect_equal(runit_qlogis(x, a, b),
                c(qlogis(x, a, b, lower=TRUE, log=FALSE),  qlogis(log(x), a, b, lower=TRUE,  log=TRUE),
                  qlogis(x, a, b, lower=FALSE, log=FALSE), qlogis(log(x), a, b, lower=FALSE, log=TRUE))
                  )
})

test_that( "(dpq)nbeta works",{
    x <- 5
    a <- 10
    b <- 7
    c <- 8
    expect_equal(runit_dnbeta(x, a, b, c),
                c(dbeta(x, a, b, c, log=FALSE), dbeta(x, a, b, c, log=TRUE))
                )

    expect_equal(runit_pnbeta(x, a, b, c),
                c(pbeta(x, a, b, c, lower=TRUE, log=FALSE),  pbeta(log(x), a, b, c, lower=TRUE, log=TRUE),
                  pbeta(x, a, b, c, lower=FALSE, log=FALSE), pbeta(log(x), a, b, c, lower=FALSE, log=TRUE))
                  )

    x <- x/a
    expect_equal(runit_qnbeta(x, a, b, c),
                c(qbeta(x, a, b, c, lower=TRUE, log=FALSE),  qbeta(log(x), a, b, c, lower=TRUE,  log=TRUE),
                  qbeta(x, a, b, c, lower=FALSE, log=FALSE), qbeta(log(x), a, b, c, lower=FALSE, log=TRUE))
                  )
})

test_that( "(dpq)nf works", {
    x <- 5
    a <- 10
    b <- 7
    c <- 8
    expect_equal(runit_dnf(x, a, b, c),
                c(df(x, a, b, c, log=FALSE), df(x, a, b, c, log=TRUE))
                )

    expect_equal(runit_pnf(x, a, b, c),
                c(pf(x, a, b, c, lower=TRUE, log=FALSE),  pf(log(x), a, b, c, lower=TRUE, log=TRUE),
                  pf(x, a, b, c, lower=FALSE, log=FALSE), pf(log(x), a, b, c, lower=FALSE, log=TRUE))
                  )

    x <- x/a
    expect_equal(runit_qnf(x, a, b, c),
                c(qf(x, a, b, c, lower=TRUE, log=FALSE),  qf(log(x), a, b, c, lower=TRUE,  log=TRUE),
                  qf(x, a, b, c, lower=FALSE, log=FALSE), qf(log(x), a, b, c, lower=FALSE, log=TRUE))
                  )

    v <- (1:9)/10
    expect_equal(runit_pnf_2(v),
                list(lowerNoLog = pf(v, 6, 8, ncp=2.5, lower=TRUE, log=FALSE),
                     lowerLog   = pf(v, 6, 8, ncp=2.5, log=TRUE ),
                     upperNoLog = pf(v, 6, 8, ncp=2.5, lower=FALSE),
                     upperLog   = pf(v, 6, 8, ncp=2.5, lower=FALSE, log=TRUE)
                     )
                     )
})

test_that( "(dpq)nt works", {
    x <- 5
    a <- 10
    b <- 7
    expect_equal(runit_dnt(x, a, b),
                c(dt(x, a, b, log=FALSE), dt(x, a, b, log=TRUE))
                )

    expect_equal(runit_pnt(x, a, b),
                c(pt(x, a, b, lower=TRUE, log=FALSE),  pt(log(x), a, b, lower=TRUE, log=TRUE),
                  pt(x, a, b, lower=FALSE, log=FALSE), pt(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    x <- x/a
    expect_equal(runit_qnt(x, a, b),
                c(qt(x, a, b, lower=TRUE, log=FALSE),  qt(log(x), a, b, lower=TRUE,  log=TRUE),
                  qt(x, a, b, lower=FALSE, log=FALSE), qt(log(x), a, b, lower=FALSE, log=TRUE))
                  )

	v <- seq(0.0, 1.0, by=0.1)
    expect_equal(runit_pnt_2(v),
                list(lowerNoLog = pt(v, 5, ncp=7),
                     lowerLog   = pt(v, 5, ncp=7,              log=TRUE),
                     upperNoLog = pt(v, 5, ncp=7, lower=FALSE),
                     upperLog   = pt(v, 5, ncp=7, lower=FALSE, log=TRUE) )
                     )
})

test_that( "(dpq)wilcox works", {
    x <- 2
    a <- 4
    b <- 6
    expect_equal(runit_dwilcox(x, a, b),
                c(dwilcox(x, a, b, log=FALSE), dwilcox(x, a, b, log=TRUE))
                )

    expect_equal(runit_pwilcox(x, a, b),
                c(pwilcox(x, a, b, lower=TRUE, log=FALSE),  pwilcox(log(x), a, b, lower=TRUE, log=TRUE),
                  pwilcox(x, a, b, lower=FALSE, log=FALSE), pwilcox(log(x), a, b, lower=FALSE, log=TRUE))
                  )

    x <- x/a
    expect_equal(runit_qwilcox(x, a, b),
                c(qwilcox(x, a, b, lower=TRUE, log=FALSE),  qwilcox(log(x), a, b, lower=TRUE,  log=TRUE),
                  qwilcox(x, a, b, lower=FALSE, log=FALSE), qwilcox(log(x), a, b, lower=FALSE, log=TRUE))
                  )
})




