package io.micronaut.build.examples;

import io.micronaut.context.annotation.Context;

@Context
final class UnsatisfiedDependencyBean {
    UnsatisfiedDependencyBean(MissingDependency missingDependency) {
    }
}
