# Bayesian Hierarchical Hidden Markov Models applied to financial time series

This project is part of [the R Project in Google Summer of Code 2017](https://github.com/rstats-gsoc/gsoc2017). The goal is to replicate research in Hierarchical Hidden Markov Models (HHMM) applied to financial data. This model is a generalization of Hidden Markov Models (HMM), which in turn are part of the Dynamic Bayesian Networks (DBN) family. We identified four academic works with interesting ideas and application that do not provide data nor code. At least three of these four candidates works will be replicated as part of this project. Replication will allow future readers to assess the credibility of the results and will work as a shortcut for those wanting to integrate this model into current research code (for example, for backtesting).

## Secondary goals ##

The concept of hidden states could enrich many trading strategies. A detailed replication that provides literature review, reproducible code and solid documentation will allow future readers to implement HHMM logic into existing trading frameworks (for example as covariates, signals and benchmarks). In other words, published code as part of this project may in a future leverage the implementation of hidden state models to already existing R packages. While the development of such a general framework is not part of this project, the delivery will be written with this eventual future enterprise in mind.

Additionally, the code and the report published as part of this project could be adapted and/or expanded for other educational purposes. While we do not expect this to happen within the project lifetime, the delivery will be written with the intention of serving as a first draft for the later development of a Case Study to be proposed to the Stan Development Team.

## Brief introduction

Jangmin et al. (2004) first proposed a HHMM to mimic dynamics of price trends in the stock markets. Hassan (2005) is one of the most popular original works that proposed a HMM for financial data. Based on daily data, they use four latent states to forecast stock market closing prices. Tayal (2009) builds upon these basic ideas and proposes a HHMM for high frequency data. Their work focus mainly on the statistical side of the model, with great emphasis on data description, inference and goodness of fit assessment. Finally, Sandoval and Hernández (2015) propose a very concrete application of HHMM to high frequency trading of foreign currency. The paper aims almost exclusively at building and assessing a trading strategy based on hierarchical hidden states.

Although the selected works develop different financial models in diverse market contexts (asset, exchange, frequency and strategy), they share the underlying statistical logic: model parameters are time variant and change according to unobservable discrete market states. As for the replications, this statistical common core is a strong incentive to write reusable code that will naturally allow for a certain degree of generalization.

## Papers

* [A literature review of Bayesian Hierarchical Hidden Markov Models applied to financial time series](litreview/main.pdf).
* [Input-Output Hidden Markov Model](iohmm-mix/main.html), a replication of Hassan (2005).

## Running the replications

All the work is organized in a few folders at root level:

* [common](common) contains general purpose files.
* [litreview](litreview) is a theoretical review of the HMM family. Read our [literature review](litreview/main.pdf).
* [hmm](hmm) includes working code that generates simulated data from a HMM and a MCMC sampler. See the [main.R](hmm/main.R) for step-by-step code.
* [iohmm-reg](iohmm-reg) includes working code that generates simulated data from a IOHMM and a MCMC sampler for fully bayesian estimation and inference. In this implementation, the observation model is a linear regression that maps the inputs to the outputs according to a set of parameters that change according to the hidden states, which in turn follow a multinomial (softmax) regression. See [main.R](iohmm-reg/main.R) for step-by-step code.
* [iohmm-mix](iohmm-mix) includes working code that generates simulated data from a IOHMM and a MCMC sampler for fully bayesian estimation and inference. In this implementation, the observation model is a mixture of Gaussians with different components per hidden state, which in turn follows a multinomial (softmax) regression. Read [our paper](iohmm-mix/main.html) and see [main.R](iohmm-mix/main.R) for step-by-step code.
* [hassan2005](hassan2005/) contains the code and the report for the replication of Hassan (2005).

Each folder may have inner folders for R, Stan and RMarkdown code.

### Prerequisites
  * R 3.3.3
  * RStudio Desktop 1.0.136
  * Rtools 3.3 (R 3.2.x to 3.3.x)
  * Stan 2.14
  * R Packages
    * RStan 2.14.2
    * [to be completed]

## Contributing

Reach us at #r-finance (freenode.net).

## Authors

* **Luis Damiano** - *Main researcher* - [luisdamiano](https://github.com/luisdamiano)
* **Brian Peterson** - *Co-Mentor* - [braverock](https://github.com/braverock)
* **Michael Weylandt** - *Co-Mentor* - [michaelweylandt](https://github.com/michaelweylandt)

## License
_A literature review of Bayesian Hierarchical Hidden Markov Models applied to financial time series_ is licensed under CC-BY-SA 4.0. See the [LICENSE](LICENSE.md) file for details.

## Acknowledgments
* The Google Summer Of Code (GSOC) program for funding.
* Brian Peterson for being a great project leader and Michael Weylandt for being awesomely bayesian. I am very grateful for the immense ammount of useful discussions and valuable feedback.
* The members of the R in Finance community for making this a warming community.
* The members of the Stan Development Team for being so passionate about bayesian statistics and technically unbeatable. I am conviced that Stan will be regarded as one of the most important contributions in this decade to the development of bayesian statistics.

## References
Hassan, M. R., & Nath, B. (2005). Stock market forecasting using hidden Markov model: a new approach. In Intelligent Systems Design and Applications, 2005. ISDA'05. Proceedings. 5th International Conference on (pp. 192-196). IEEE.

Jangmin, O., Lee, J., Park, S. B., & Zhang, B. T. (2004). Stock trading by modelling price trend with dynamic Bayesian networks. Intelligent Data Engineering and Automated Learning–IDEAL 2004, 794-799.

Sandoval, J., & Hernández, G. (2015). Computational Visual Analysis of the Order Book Dynamics for Creating High-frequency Foreign Exchange Trading Strategies. Procedia Computer Science, 51, 1593-1602.

Stan Development Team (2016). Stan Modeling Language: User’s Guide and Reference Manual. Version 2.14.0.

Tayal, A. (2009). Regime switching and technical trading with dynamic Bayesian networks in high-frequency stock markets (Master dissertation, University of Waterloo).
