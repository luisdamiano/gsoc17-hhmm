# Bayesian Hierarchical Hidden Markov Models applied to financial time series

## Results of our research replication

Read online our resulting write-ups:

* [A brief technical introduction to Hidden Markov Models](techreview/main.pdf).
* [Input-Output Hidden Markov Model applied to financial time series](hassan2005/main.html), a replication of Hassan (2005).
* [Regime Switching and Technical Trading with Dynamic Bayesian Networks in High-Frequency Stock Markets](tayal2009/main.pdf), a replication of Tayal (2009).

Read below to know more about our project!

## Primary goals

This project is part of [the R Project in Google Summer of Code 2017](https://github.com/rstats-gsoc/gsoc2017). The goal is to replicate research in Hierarchical Hidden Markov Models (HHMM) applied to financial data. This model is a generalization of Hidden Markov Models (HMM), which in turn are part of the Dynamic Bayesian Networks (DBN) family. We identified four academic works with interesting ideas and applications that do not provide data nor code. Two of these four candidates works were replicated as part of this project. Replication will allow future readers to assess the credibility of the results and will work as a shortcut for those wanting to integrate this model into current research code (for example, for backtesting). We also produce a brief summary of the mathematical treatment of HMM.

## Secondary goals ##

The concept of hidden states could enrich many trading strategies. A detailed replication that provides literature review, literate programming and reproducible code will allow future readers to implement HHMM logic into existing trading frameworks (for example as covariates, signals and benchmarks). In other words, already existing R packages may in a future leverage on the code published as part of this project. While the development of such a general framework is not part of this project, the delivery will be written with this eventual future enterprise in mind.

Additionally, the code and the report published as part of this project could be adapted and/or expanded for other educational purposes. While we did not expect this to happen within the project lifetime, the delivery was written with the intention of serving as a first draft for the later development of a Case Study to be proposed to the Stan Development Team.

## Brief introduction

Jangmin et al. (2004) first proposed a HHMM to mimic dynamics of price trends in the stock markets. Hassan (2005) is one of the most popular original works that proposed a HMM for financial data. Based on daily data, they use four latent states to forecast stock market closing prices. Tayal (2009) builds upon technical analysis concepts and proposes a very interesting feature extraction procedure and its corresponding HHMM for high frequency data. His work focuses mainly on the statistical side of the model, with great emphasis on data description, inference and goodness of fit assessment. Finally, Sandoval and Hernández (2015) propose a very concrete application of HHMM to high frequency trading of foreign currency. The paper improves Tayal's treatment of the change in trade volume.

Although the selected works develop different financial models in diverse market contexts (asset, exchange, frequency and strategy), they share the underlying statistical logic: model parameters are time variant and change non-linearly according to unobservable discrete market states. As for the replications, this statistical common core is a strong incentive to write reusable code that will naturally allow for a certain degree of generalization.

## Running the replications

All the work is organized in a few folders at root level:

* [common](common) contains general purpose files.
* [techreview](techreview) is a techinical review of the HMM family. Read our [brief technical introduction to HHM](techreview/main.pdf).
* [hmm](hmm) includes working code that generates simulated data from a HMM, as well as MCMC samplers for HMM with Multinomial or Gaussian observations. We also provide a sampler for a very specific case of semi-supervised learning. See the [main.R](hmm/main.R), [main-multinorm.R](hmm/main-multinorm.R) and [main-multinorm-semisup.R](hmm/main-multinorm-semisup.R) for step-by-step code.
* [iohmm-reg](iohmm-reg) includes working code that generates simulated data from a IOHMM and a MCMC sampler for fully Bayesian estimation and inference. In this implementation, the observation model is a linear regression that maps the inputs to the outputs according to a set of parameters that change according to the hidden states, which in turn follow a multinomial (softmax) regression. See [main.R](iohmm-reg/main.R) for step-by-step code.
* [iohmm-mix](iohmm-mix) includes working code that generates simulated data from a IOHMM and a MCMC sampler for fully Bayesian estimation and inference. In this implementation, the observation model is a mixture of Gaussians with different components per hidden state, which in turn follows a multinomial (softmax) regression. Read [our paper](iohmm-mix/main.html) and see [main.R](iohmm-mix/main.R) for step-by-step code.
* [hhmm](hhmm) includes a small set of S3 objects with recursive generic methods very useful to set up a HHMM structure and draw samples from the model. Several examples based on actual papers are provided in the root folder: [sim-fine1998.R](hhmm/sim-fine1998.R) and [sim-jangmin2004.R](hhmm/sim-jangmin2004.R). See [main.R](hhmm/main.R) for step-by-step code.
* [hassan2005](hassan2005/) contains the code and the write-up for the replication of Hassan (2005). See [main.R](hassan2005/main.R) for step-by-step code and [the write-up](hassan2005/main.html).
* [tayal2009](tayal2009/) contains the code and the write-up for the replication of Tayal (2009). See [main.R](hhmm/main.R) for step-by-step code and [the write-up](tayal2009/main.html)..

Each folder may have inner folders for R, Stan and RMarkdown code.

### Prerequisites
  * R 3.3.3
  * RStudio Desktop 1.0.136
  * Rtools 3.3 (R 3.2.x to 3.3.x)
  * Stan 2.14
  * R Packages
    * RStan 2.14.2

## Contributing

Reach us at #r-finance (freenode.net).

## Authors

* **Luis Damiano** - *Main researcher* - [luisdamiano](https://github.com/luisdamiano)
* **Brian Peterson** - *Co-Mentor* - [braverock](https://github.com/braverock)
* **Michael Weylandt** - *Co-Mentor* - [michaelweylandt](https://github.com/michaelweylandt)

## License
_Bayesian Hierarchical Hidden Markov Models applied to financial time series_ is licensed under CC-BY-SA 4.0. See the [LICENSE](LICENSE.md) file for details.

## Acknowledgments
* The Google Summer Of Code (GSOC) program for funding.
* Brian Peterson for being a great project leader and Michael Weylandt for being awesomely Bayesian and contributing with great statistical insight. I am very grateful for the immense amount of useful discussions and valuable feedback. I am forever in debt for their enormous vote of confidence, both inside and outside this project. This may very well be a life changer.
* The members of the R in Finance community for working so hard on building this a warming community.
* The members of the Stan Development Team for being so passionate about Bayesian statistics and technically unbeatable. Special mentions to Aaron Goodman, Ben Bales and Bob Carpenter for their active participation in the discussions held in Stan forums for [HMM with constraints](http://discourse.mc-stan.org/t/hidden-markov-model-with-constraints/1625/4) and [HMMM](http://discourse.mc-stan.org/t/transversing-up-a-graph-hierarchical-hidden-markov-model/1304/11). I am convinced that Stan will be regarded as one of the most important contributions in this decade to the development of Bayesian statistics.
* Most importantly, my family, soon-to-be wife, friends and co-workers that gave me so much support during this extremely intensive three-month journey.

## References
Hassan, M. R., & Nath, B. (2005). Stock market forecasting using hidden Markov model: a new approach. In Intelligent Systems Design and Applications, 2005. ISDA'05. Proceedings. 5th International Conference on (pp. 192-196). IEEE.

Jangmin, O., Lee, J., Park, S. B., & Zhang, B. T. (2004). Stock trading by modelling price trend with dynamic Bayesian networks. Intelligent Data Engineering and Automated Learning–IDEAL 2004, 794-799.

Sandoval, J., & Hernández, G. (2015). Computational Visual Analysis of the Order Book Dynamics for Creating High-frequency Foreign Exchange Trading Strategies. Procedia Computer Science, 51, 1593-1602.

Stan Development Team (2017) Stan Modeling Language: User’s Guide and Reference Manual. Version 2.16.0.

Tayal, A. (2009). Regime switching and technical trading with dynamic Bayesian networks in high-frequency stock markets (Master dissertation, University of Waterloo).
