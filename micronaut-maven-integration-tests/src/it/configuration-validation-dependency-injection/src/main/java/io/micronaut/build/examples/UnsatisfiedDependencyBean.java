package io.micronaut.build.examples;

import jakarta.inject.Singleton;

@Singleton
final class UnsatisfiedDependencyBean {
    UnsatisfiedDependencyBean(MissingDependency missingDependency) {
    }
}
