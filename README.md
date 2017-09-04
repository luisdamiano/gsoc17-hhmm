# Bayesian Hierarchical Hidden Markov Models applied to financial time series

## Results of our research replication

Read online our resulting write-ups:

* [A brief technical introduction to Hidden Markov Models](https://luisdamiano.github.io/gsoc17/hmm_techreview.pdf).
* [Input-Output Hidden Markov Model applied to financial time series](https://luisdamiano.github.io/gsoc17/iohmm_financial_time_series.html), a replication of Hassan (2005).
* [Regime Switching and Technical Trading with Dynamic Bayesian Networks in High-Frequency Stock Markets](https://luisdamiano.github.io/gsoc17/rs_technical_trading.pdf), a replication of Tayal (2009).

Read below to know more about our project!

## Primary goals

This project is part of [the R Project in Google Summer of Code 2017](https://github.com/rstats-gsoc/gsoc2017). The goal is to replicate research in Hierarchical Hidden Markov Models (HHMM) applied to financial data. This model is a generalization of Hidden Markov Models (HMM), which in turn are part of the Dynamic Bayesian Networks (DBN) family. We identified four academic works with interesting ideas and applications that do not provide data nor code. Two of these four candidates works were replicated as part of this project. Replication will allow future readers to assess the credibility of the results and will work as a shortcut for those wanting to integrate this model into current research code (for example, for backtesting). We also produce a brief summary of the mathematical treatment of HMM.

## Secondary goals

The concept of hidden states could enrich many trading strategies. A detailed replication that provides literature review, literate programming and reproducible code will allow future readers to implement HHMM logic into existing trading frameworks (for example as covariates, signals and benchmarks). In other words, already existing R packages may in a future leverage on the code published as part of this project. While the development of such a general framework is not part of this project, the delivery will be written with this eventual future enterprise in mind.

Additionally, the code and the report published as part of this project could be adapted and/or expanded for other educational purposes. While we did not expect this to happen within the project lifetime, the delivery was written with the intention of serving as a first draft for the later development of a Case Study to be proposed to the Stan Development Team.

## Brief introduction

Jangmin et al. (2004) first proposed a HHMM to mimic dynamics of price trends in the stock markets. Hassan (2005) is one of the most popular original works that proposed a HMM for financial data. Based on daily data, they use four latent states to forecast stock market closing prices. Tayal (2009) builds upon technical analysis concepts and proposes a very interesting feature extraction procedure and its corresponding HHMM for high frequency data. His work focuses mainly on the statistical side of the model, with great emphasis on data description, inference and goodness of fit assessment. Finally, Sandoval and Hernández (2015) propose a very concrete application of HHMM to high frequency trading of foreign currency. The paper improves Tayal's treatment of the change in trade volume.

Although the selected works develop different financial models in diverse market contexts (asset, exchange, frequency and strategy), they share the underlying statistical logic: model parameters are time variant and change non-linearly according to unobservable discrete market states. As for the replications, this statistical common core is a strong incentive to write reusable code that will naturally allow for a certain degree of generalization.

## Running the replications

We encourage the reader to try the code by themselves, possibly using data of their own interest.

All the work is organized in a few folders at root level:

* [common](common) contains general purpose files.
* [techreview](techreview) is a technical review of the HMM family. Read our [brief technical introduction to HHM](https://luisdamiano.github.io/gsoc17/hmm_techreview.pdf).
* [hmm](hmm) includes working code that generates simulated data from a HMM, as well as MCMC samplers for HMM with Multinomial or Gaussian observations. We also provide a sampler for a very specific case of semi-supervised learning. See the [main.R](hmm/main.R), [main-multinorm.R](hmm/main-multinorm.R) and [main-multinorm-semisup.R](hmm/main-multinorm-semisup.R) for step-by-step code.
* [iohmm-reg](iohmm-reg) includes working code that generates simulated data from a IOHMM and a MCMC sampler for fully Bayesian estimation and inference. In this implementation, the observation model is a linear regression that maps the inputs to the outputs according to a set of parameters that change according to the hidden states, which in turn follow a multinomial (softmax) regression. See [main.R](iohmm-reg/main.R) for step-by-step code.
* [iohmm-mix](iohmm-mix) includes working code that generates simulated data from a IOHMM and a MCMC sampler for fully Bayesian estimation and inference. In this implementation, the observation model is a mixture of Gaussians with different components per hidden state, which in turn follows a multinomial (softmax) regression. Read [our paper](iohmm-mix/main.html) and see [main.R](iohmm-mix/main.R) for step-by-step code.
* [hhmm](hhmm) includes a small set of S3 objects with recursive generic methods very useful to set up a HHMM structure and draw samples from the model. Several examples based on actual papers are provided in the root folder: [sim-fine1998.R](hhmm/sim-fine1998.R) and [sim-jangmin2004.R](hhmm/sim-jangmin2004.R). See [main.R](hhmm/main.R) for step-by-step code.
* [hassan2005](hassan2005/) contains the code and the write-up for the replication of Hassan (2005). See [main.R](hassan2005/main.R) for step-by-step code and [the write-up](https://luisdamiano.github.io/gsoc17/iohmm_financial_time_series.html).
* [tayal2009](tayal2009/) contains the code and the write-up for the replication of Tayal (2009). See [main.R](hhmm/main.R) for step-by-step code and [the write-up](https://luisdamiano.github.io/gsoc17/rs_technical_trading.pdf).

Each folder may have inner folders for R, Stan and RMarkdown code.

### Prerequisites
  * R 3.3.3
  * RStudio Desktop 1.0.136
  * Rtools 3.3 (R 3.2.x to 3.3.x)
  * Stan 2.14
  * R Packages
    * RStan 2.14.2

## Contributing

Reach us at [#r-finance](http://webchat.freenode.net/?channels=r-finance) (freenode.net).

## Authors

* **Luis Damiano** - *Main researcher* - [luisdamiano](https://github.com/luisdamiano)
* **Brian Peterson** - *Co-Mentor* - [braverock](https://github.com/braverock)
* **Michael Weylandt** - *Co-Mentor* - [michaelweylandt](https://github.com/michaelweylandt)

## License
_Bayesian Hierarchical Hidden Markov Models applied to financial time series_ is licensed under CC-BY-SA 4.0. See the [LICENSE](LICENSE.md) file for details.

## Acknowledgments
* The Google Summer Of Code (GSOC) program for funding.
* Brian Peterson for being a great project leader and Michael Weylandt for being awesomely Bayesian and contributing with great statistical insight. I am very grateful for the immense amount of useful discussions and valuable feedback. I am forever in debt for their enormous vote of confidence, both inside and outside this project. This may very well be a life changer.
* The members of the R in Finance community for working so hard to build this warming community.
* The members of the Stan Development Team for being so passionate about Bayesian statistics and technically unbeatable. Special mentions to Aaron Goodman, Ben Bales and Bob Carpenter for their active participation in the discussions held in Stan forums for [HMM with constraints](http://discourse.mc-stan.org/t/hidden-markov-model-with-constraints/1625/4) and [HMMM](http://discourse.mc-stan.org/t/transversing-up-a-graph-hierarchical-hidden-markov-model/1304/11). I am convinced that Stan will be regarded as one of the most important contributions in this decade to the development of Bayesian statistics.
* Most importantly, my family, soon-to-be wife, friends and co-workers that gave me so much support during this extremely intensive three-month journey.

# On our GSoC Project

## In the beginning...

I first heard about GSoC on March 2017 from Brian Petersen. A long term contributor to the R Programming language with a prominent role in the R/Finance community, he has successfully mentored students under this program to produce Open Source software. As a student in applied statistics with working experience in Corporate Finance and Asset Management, I enjoy doing research on quantitative topics related to financial markets, time series, Bayesian statistics, R and Stan. We agreed that research replication on Hierarchical Hidden Markov Models applied to finance time series would be fruitful from the computational and the finance point of view, providing the R community with both software and analysis based on domain knowledge.

He introduced me to Michael, a well rounded PhD student in Statistics at Rice University with experience in Bayesian statistics, R, Stan and financial applications. Although he first warned us that we may be setting the bar too high for a summer project, he said that the attempt was worthwhile and agreed to team up.

## First steps

I started the literature review before being accepted for the project. I believed that the review would be useful for my personal future research even if the proposal had not been accepted. This proved to be key as time turned to be our most cruel constraint. I was fortunate that my school had prepared me well to do research on my own, interaction at this point in time was still incipient.

In preparation for the actual papers, I first coded some [basic routines for HMM](hmm/stan) in the Stan programming language. I am very grateful to the whole Stan Development Team as I relied greatly on the Stan manual (Stan Development Team 2017). Besides, I created R code for [diagnostics and visualizations](common/R/plots.R) as well as other [common code for Rmarkdown](common/Rmd). Visualization functions would later prove very useful for diagnostics and effectively sped up the writing stage.

Next, I started with Hassan (2005). Following the calibration by simulation methodology (Cook, Gelman, and Rubin 2006), I wrote a few routines in R to [draw simulated samples from several IOHMM variations](iohmm-reg/R/iohmm-sim.R). I had read about this approach on blogs and other information sources, but Michael pointed me out the paper as a more robust framework. After coding the models in Stan language ([IOHMM with regression model](iohmm-reg/stan/iohmm-reg.stan) and [IOHMM with mixture gaussian](iohmm-mix/stan/iohmm-mix.stan) variants), I run many diagnostics to ensure that the software would recover the parameters correctly. As the sampler suffered from divergences, I devised an alternative [IOHMM with a hierarchical mixture gaussian](iohmm-mix/stan/iohmm-hmix.stan) that vastly improved estimation efficiency.

Confident that our software worked as intended, we finally studied real data. Quantmod (Ryan 2008) made [data adquisition and pre-processing](hassan2005/R/data.R) trivial. We obtained [satisfying results from our model](hassan2005/main.R) and worked on comprehensive [write-up](hassan2005/main.Rmd).

Naturally, the following step was the second replication: Jangmin (2009). It involves a semi-supervised HHMM that seemed overly complicated at first glance. After many days trying to write programming routines that would reflect the model truthfully, I sent an e-mail to my mentors with the subject _Stuck!_. This is where being supported by good mentors makes the difference. Michael studied the paper with me and we realised that most of the complexity was, in fact, lack of clarity in the definitions and the methodology. Moreover, during the discussion we observed that the article proposed an ad-hoc solution to a very specific situation, thus becoming of little interest to the broad R community. Consequently, Michael, Brian and I agree to skip this work and head for the next.

No doubt, Tayal (2009) looked challenging from the very beginning. The [feature engineering procedure](tayal2009/R/feature-extraction.R), certainly very interesting from the financial point of view, was described thoroughly but involved a good deal of coding time nonetheless. We also anticipated that working with high-frequency data would increase dramatically the time we needed to run our computations. Finally, we expected the generalized forward algorithm to be difficult to implement in Stan and, thanks to the insight provided by Michael, we decided to reestate the HHMM into an equivalent HMM. After a detailed study of model constraints, I succeeded in simplifying the HHMM into a HMM with only three free parameters for the hidden dynamics. Again, we tested our software with [simulations](tayal2009/main-sim.R).

Although implementing the generalized algorithms would be a very worthwhile enterprise on its own, [our approximation](tayal2009/stan/hhmm-tayal2009.stan) was acceptable given the amount of data and the highly constrained HHMM we were working with. Furthermore, we created [a faster sampler](tayal2009/stan/hhmm-tayal2009-lite.stan) for walk-forward backtesting that would implement only the bare minimum computations needed, leaving many other useful and interpretable quantities present in our full sampler. Implementing in Stan a fully Bayesian version of all the algorithms involved in a HHMM, mostly based on the original work of Fine (1988), would be an involved enough project to be considered a part of the contributions required by a PhD program.

Brian offered to acquire the data as the high frequency dataset used in the original article was not publicly available. Data processing was trivial thanks to [a parser in the FinancialInstrument](https://github.com/cran/FinancialInstrument/blob/d054f8b2d334b59c5fab4623fb15c6ecbc06b7ce/inst/parser/TRTH_BackFill.R) R package (Carl, Peterson and See 2014). I only had to address a few issues as I was not using a Unix platform (won't let this happen again, sorry!).

## Interaction with mentors
We agreed on a rich interaction methodology:

- Very frequent, almost daily, git commits.
- A [personal log](log.md) with time stamps, questions, notes and other wild thoughts. This allowed the mentors to ~~read into my mind~~ follow closely the development of the research even if our personal schedules didn't match.
- A periodic check-in. We loosely adhered to a weekly e-mail plus a few Hangout sessions.
- My GSoC submission and their feedback at each evaluation deadline.

We were also involved in several aperiodic, on-demand interaction sessions (mostly e-mails and chats).

## In restrospective...

I am very happy with the whole GSoC experience. Long programming hours improved my R and Stan skills greatly, but I am mostly grateful for many other learning opportunities: teaming up for collaborative programming, creating and maintaining a github repository, interacting with the open source community and discussing my ideas with skillful and experienced mentors.

I would recommend any student interested in software development and programming languages to take up a GSoC Project. Just send an e-mail to your favourite mailing list asking for a mentor. In the very rare case where nobody volunteers, you can ask for further guidance in [our chatroom](http://webchat.freenode.net/?channels=r-finance).

If I had to give future students one piece of advice it would be this: make sure you choose a topic you really like. This way, you will enjoy your GSoC journey as much as I did.

# References

Carl, P., Peterson, B. G., & See, G (2014). FinancialInstrument: Financial Instrument Model Infrastructure for R. R package version 1.2.0. URL https://CRAN.R-project.org/package=FinancialInstrument.

Cook, S. R., Gelman, A., & Rubin, D. B. (2006). Validation of software for Bayesian models using posterior quantiles. Journal of Computational and Graphical Statistics, 15(3), 675-692.

Hassan, M. R., & Nath, B. (2005). Stock market forecasting using hidden Markov model: a new approach. In Intelligent Systems Design and Applications, 2005. ISDA'05. Proceedings. 5th International Conference on (pp. 192-196). IEEE.

Jangmin, O., Lee, J., Park, S. B., & Zhang, B. T. (2004). Stock trading by modelling price trend with dynamic Bayesian networks. Intelligent Data Engineering and Automated Learning–IDEAL 2004, 794-799.

Ryan, J. A. (2008). quantmod: Quantitative Financial Modelling Framework. R package version 0.4-10. URL https://CRAN.R-project.org/package=quantmod.

Sandoval, J., & Hernández, G. (2015). Computational Visual Analysis of the Order Book Dynamics for Creating High-frequency Foreign Exchange Trading Strategies. Procedia Computer Science, 51, 1593-1602.

Stan Development Team (2017). Stan Modeling Language: User’s Guide and Reference Manual. Version 2.16.0.

Tayal, A. (2009). Regime switching and technical trading with dynamic Bayesian networks in high-frequency stock markets (Master dissertation, University of Waterloo).
