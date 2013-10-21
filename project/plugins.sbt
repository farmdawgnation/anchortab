resolvers += Resolver.url("artifactory", url("http://scalasbt.artifactoryonline.com/scalasbt/sbt-plugin-releases"))(Resolver.ivyStylePatterns)

addSbtPlugin("com.earldouglas" % "xsbt-web-plugin" % "0.4.2")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.10.0")

addSbtPlugin("com.openstudy" % "sbt-resource-management" % "0.4.1-SNAPSHOT")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.7.4")
