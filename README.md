# Anchor Tab

This is Anchor Tab. This is a project that I engaged in with some college friends of mine
who collectively were known as The Cirion Group. Over the course of two years we used our
spare time to architect, build, release, and run a minimalistic email marketing opt-in tool.
We built something that we felt was desperately lacking in the market, and many of our customers
agreed.

We were profitable nearly from day 1, though a lack of marketing effort and experience
on our end meant those profits remained small. As interest in the project dried up and it
became apparent that we couldn't continue to maintain it, we shut the project down in 2014.
We have open sourced the project under the Apache 2.0 license, in accordance with our partnership
agreement at the time the product was started.

The application is built in Scala and runs on the Lift Framework. It integrates with:

* Mandrill for transactional email delivery.
* MailChimp, Campaign Monitor, ConstantContact, and Pardot to add subscribers on behalf of customers.
* Stripe for billing.
* Google Analytics and Mixpanel for analytics.
* Amazon S3 for static asset deployment.

All API keys that are used in this code have been deleted or deactivated. If you wish to use
the application yourself, you'll need to register developer accounts with the relevant third
parties and get your own API keys.

## Running the app

At a minimum, to play with the application locally you'll need to:

* Install compass.
* Install coffeescript.
* Install sbt.
* Install mongodb.
* Get a Stripe API key so the app can create customers and set it up in
  default.props.
* Get API from whichever marketing services you'll want to play with and set
  it up in default.props.

Once you have these things done, navigate to the project directory in your terminal and
execute the following commands:

```
$ sbt
> resources:compile-sass
> resources:copy-scripts
> container:start
```

If everything is configured correctly, this should get you a functioning copy of the
application running on port 8081.

## Interesting bits

There are lots of goodies in this code for anyone interested in exploring a Lift/Scala
application. A few particular points of note:

* How I bootstrapped an embedded jetty so that the app could be distributed as a single JAR file in the
  [Start.scala](https://github.com/farmdawgnation/anchortab/blob/master/src/main/scala/bootstrap/Start.scala) file. This was
  a variation on a solution that I found online and had to do some improvement upon to get working. You can even see where
  I started to write code to allow for downtime-less reload and never finished.
* Automated database migrations on boot in [SetupDb.scala](https://github.com/farmdawgnation/anchortab/blob/master/src/main/scala/bootstrap/liftweb/SetupDb.scala)
* How I implemented API abstractions for the various services. These all leverage databinder-dispatch
  for talking over HTTP to the remote services and lift-json for dealing with the actual JSON data.
  * [MailChimp's OAuth](https://github.com/farmdawgnation/anchortab/blob/master/src/main/scala/com/anchortab/mailchimp/MailchimpOAuth.scala)
  * [ConstantContact's API](https://github.com/farmdawgnation/anchortab/blob/master/src/main/scala/com/anchortab/constantcontact/ConstantContact.scala)
  * [Campaign Monitor's API](https://github.com/farmdawgnation/anchortab/blob/master/src/main/scala/com/anchortab/campaignmonitor/CampaignMonitor.scala)
* The implementation of our REST API using Lift's RestHelper in [Api.scala](https://github.com/farmdawgnation/anchortab/blob/master/src/main/scala/com/anchortab/snippet/Api.scala)
* The [NeighborhoodWatchActor](https://github.com/farmdawgnation/anchortab/blob/master/src/main/scala/com/anchortab/actor/NeighborhoodWatchActor.scala) that
  was responsible for alerting us to potential abuse of free accounts to get around quotas.
* Our [StripeHook](https://github.com/farmdawgnation/anchortab/blob/master/src/main/scala/com/anchortab/snippet/StripeHook.scala) that received notifications
  from Stripe and made changes in our system due to changes in payment status and so forth. It was responsible for figuring out how certain events in
  Stripe (e.g. billing a credit card failed) should be handled from our end (e.g. disable the subscription, stop showing tabs).
* The [ServiceWrappers](https://github.com/farmdawgnation/anchortab/blob/master/src/main/scala/com/anchortab/model/ServiceWrapper.scala) that are saved in
  MongoDB with Tab information. This is the component that knows what API library / credentials to be using for a particular tab. Whenever an email came
  in for a Tab we would just retrieve the ServiceWrapper stored on that Tab and invoke the `subscribeEmail` method.

## Final Notes

This code is unsupported, but I do think it serves as a pretty great example of what a Lift app can
do with minimal struggle. Gotta love it.
