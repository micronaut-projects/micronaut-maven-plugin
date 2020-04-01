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

