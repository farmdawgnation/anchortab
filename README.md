# Anchor Tab

This is Anchor Tab. This is a project that I engaged in with some college friends of mine
who collectively were known as The Cirion Group. Over the course of two years we used our
spare time to architect, build, release, and run a minimalistic email marketing opt-in tool.
We were profitable nearly from day 1, though a lack of marketing effort and experience
on our end meant those profits remained small. As interest in the project dried up and it
became apparent that we couldn't continue to maintain it, we shut the project down in 2014.

We have open sourced the project under the Apache 2.0 license, in accordance with our partnership
agreement at the time the product was started. The application is built in Scala and runs
on the Lift Framework. It integrates with:

* Mandrill for transactional email delivery.
* MailChimp, Campaign Monitor, and ConstantContact to add subscribers on behalf of customers.
* Stripe for billing.
* Google Analytics and Mixpanel for analytics.
* Amazon S3 for static asset deployment.

All API keys that are used in this code have been deleted or deactivated. If you wish to use
the application yourself, you'll need to register developer accounts with the relevant third
parties and get your own API keys.

At a minimum, to play with the application locally you'll need to:

* Install sbt.
* Install mongodb.
* Get a Stripe API key so the app can create customers and set it up in
  default.props.
* Get API from whichever marketing services you'll want to play with and set
  it up in default.props.

This code is unsupported, but I do think it serves as a pretty great example of what a Lift app can
do with minimal struggle. Gotta love it.
