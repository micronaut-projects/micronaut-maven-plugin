/*
 * Copyright 2017-2021 original authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.micronaut.maven.fixtures;

import io.micronaut.openapi.generator.GeneratorOptionsBuilder;

/**
 * The options builder for {@link DummyOpenApiGenerator}.
 */
public final class DummyOptionsBuilder implements GeneratorOptionsBuilder {
    private final DummyOpenApiGenerator generator;

    public DummyOptionsBuilder(DummyOpenApiGenerator generator) {
        this.generator = generator;
    }

    public DummyOptionsBuilder withGreeting(String greeting) {
        System.out.println("Greetings from builder = " + greeting);
        generator.setGreeting(greeting);
        return this;
    }

    public DummyOptionsBuilder withJoy(boolean joy) {
        System.out.println("With joy = " + joy);
        generator.setJoy(joy);
        return this;
    }

    public DummyOptionsBuilder withMagicNumber(int magic) {
        System.out.println("With magic = " + magic);
        generator.setMagicNumber(magic);
        return this;
    }
}
