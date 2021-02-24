# Micronaut Maven Plugin

[![Maven Central](https://img.shields.io/maven-central/v/io.micronaut.build/micronaut-maven-plugin.svg?label=Maven%20Central)](https://search.maven.org/artifact/io.micronaut.build/micronaut-maven-plugin)
[![Build Status](https://github.com/micronaut-projects/micronaut-maven-plugin/workflows/Snapshot/badge.svg)](https://github.com/micronaut-projects/micronaut-maven-plugin/actions)

Maven plugin to execute Micronaut applications.

## Documentation

See the [Documentation](https://micronaut-projects.github.io/micronaut-maven-plugin/latest) for more information.

See the [Snapshot Documentation](https://micronaut-projects.github.io/micronaut-maven-plugin/snapshot) for the current development docs.

To see the documentation of a particular version, replace `latest` or `snapshot` in the above URLs with the version string.

## Snapshots and Releases

Snaphots and releases are automatically published to Maven Central using [Github Actions](https://github.com/micronaut-projects/micronaut-maven-plugin/actions).

A release is performed with the following steps:

* [Publish the draft release](https://github.com/micronaut-projects/micronaut-maven-plugin/releases). There should be already a draft release created, edit and publish it. The Git Tag should start with `v`. For example `v1.0.0`.
* [Monitor the Workflow](https://github.com/micronaut-projects/micronaut-maven-plugin/actions?query=workflow%3ARelease) to check it passed successfully.
* Celebrate!

## Contributing tips and tricks

### Running integration tests

You can run integration tests by executing `mvn verify -Prun-its`

If you want to run individual tests, you can execute `mvn verify -Prun-its "-Dinvoker.test=dockerfile*"`. In this case,
`dockerfile*` will match all test projects under `src/it` folder with a name that starts with "dockerfile".

### Debugging

To debug the plugin, you first need to publish a snapshot to your Maven local:

```shell
$ mvn install
```

Then you need a sample application. The one at `examples/java` is the most up-to-date, but you can in principle generate
a new one from Micronaut Starter. Then, change its `pom.xml` to set the following property:

```xml
<micronaut-maven-plugin.version>1.1.5-SNAPSHOT</micronaut-maven-plugin.version>
```

Pointing to whatever snapshot version you published before.

Finally, from the sample application folder, run the Maven goal you are interested into with `mvnDebug` instead of `mvn`:

```shell
mvnDebug package -Dpackaging=docker
```

Then in your IDE, attach a remote debugger to port 8000.