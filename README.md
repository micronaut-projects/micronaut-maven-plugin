# Micronaut Maven Plugin

[![Maven Central](https://img.shields.io/maven-central/v/io.micronaut.maven/micronaut-maven-plugin.svg?label=Maven%20Central)](https://search.maven.org/artifact/io.micronaut.maven/micronaut-maven-plugin)
[![Build Status](https://github.com/micronaut-projects/micronaut-maven-plugin/actions/workflows/snapshot.yml/badge.svg)](https://github.com/micronaut-projects/micronaut-maven-plugin/actions)
[![Quality Gate Status](https://sonarcloud.io/api/project_badges/measure?project=io.micronaut.maven%3Amicronaut-maven-plugin-parent&metric=alert_status)](https://sonarcloud.io/summary/new_code?id=io.micronaut.maven%3Amicronaut-maven-plugin-parent)
[![Revved up by Develocity](https://img.shields.io/badge/Revved%20up%20by-Develocity-06A0CE?logo=Gradle&labelColor=02303A)](https://ge.micronaut.io/scans)


Maven plugin to execute Micronaut applications.

## Documentation

See the [Documentation](https://micronaut-projects.github.io/micronaut-maven-plugin/latest) for more information.

See the [Snapshot Documentation](https://micronaut-projects.github.io/micronaut-maven-plugin/snapshot) for the current development docs.

To see the documentation of a particular version, replace `latest` or `snapshot` in the above URLs with the version string.

## Snapshots and Releases

Snapshots and releases are automatically published to Maven Central using [GitHub Actions](https://github.com/micronaut-projects/micronaut-maven-plugin/actions).

A release is performed with the following steps:

* [Publish the draft release](https://github.com/micronaut-projects/micronaut-maven-plugin/releases). There should be already a draft release created, edit and publish it. The Git Tag should start with `v`. For example `v1.0.0`.
* [Monitor the Workflow](https://github.com/micronaut-projects/micronaut-maven-plugin/actions?query=workflow%3ARelease) to check it passed successfully.
* Celebrate!

## Contributing tips and tricks

### Bootstrapping the toolchain

This repository's root validation and verify commands expect Java 25. The repo includes a
`.sdkmanrc`, but shared shells may still start on an older JDK until you apply it.

If you use SDKMAN!, run this from the repository root before `./mvnw ...` commands:

```shell
sdk env
java -version
```

If you do not use SDKMAN!, set `JAVA_HOME` to a Java 25 installation before running the same commands.

### Running integration tests

Run the wrapper so you inherit the repo's Maven configuration:

```shell
./mvnw verify
```

If you want to run individual tests, you can execute `./mvnw verify "-Dinvoker.test=dockerfile*"`. In this case,
`dockerfile*` will match all test projects under `src/it` folder with a name that starts with "dockerfile".

### Windows-sensitive preflight

If your change touches GitHub Actions workflows, `.github/scripts`, `.mvn/wrapper`, `mvnw`, or `mvnw.cmd`, start with the repo-owned CI-sensitive preflight wrapper before review handoff:

```shell
bash .github/scripts/ci-sensitive-preflight.sh
```

The wrapper always runs workflow pinning locally. In a Windows shell that exposes `cmd.exe`, it also dispatches the lightweight wrapper/bootstrap validation automatically.

If you want to run the Windows-specific step directly, execute:

```shell
.\.github\scripts\windows-preflight.cmd
```

If you want to run the workflow pinning step directly, execute:

```shell
bash .github/scripts/check-workflow-pinning.sh
```

If you do not have a Windows shell locally, the wrapper will remind you to open a draft PR or run the `Windows Preflight` workflow manually to get the same wrapper/bootstrap check on `windows-latest` without waiting for the full Windows CI job.

### Debugging

To debug the plugin, you first need to publish a snapshot to your Maven local:

```shell
./mvnw install
```

You can skip execution of integration tests by adding `-Dinvoker.skip=true` to the command line.

Then you need a sample application. The one at `examples/java` is the most up-to-date, but you can in principle generate
a new one from Micronaut Starter. Then, change its `pom.xml` to set the following property, using the snapshot version
you published locally. For branch-specific examples and current documentation, refer to the Snapshot Documentation link
above:

```xml
<micronaut-maven-plugin.version>X.Y.Z-SNAPSHOT</micronaut-maven-plugin.version>
```

Pointing to whatever snapshot version you published before.

Finally, from the sample application folder, run the Maven goal you are interested into with `mvnDebug` instead of `mvn`:

```shell
mvnDebug package -Dpackaging=docker
```

Then in your IDE, attach a remote debugger to port 8000.

### Preparing for a new minor/major version

```shell
./mvnw release:update-versions -DautoVersionSubmodules=true -DdevelopmentVersion=X.Y.Z-SNAPSHOT
```
