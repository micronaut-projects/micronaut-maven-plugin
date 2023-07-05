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

import io.micronaut.openapi.generator.AbstractMicronautJavaCodegen;
import org.openapitools.codegen.CodegenType;

/**
 * A dummy generator used in integration tests to make sure it is possible to extend
 * the default code generators.
 */
public final class DummyOpenApiGenerator extends AbstractMicronautJavaCodegen<DummyOptionsBuilder> {
    private String greeting;
    private boolean joy;
    private int magicNumber;

    public String getName() {
        return "Dummy Generator";
    }

    @Override
    public CodegenType getTag() {
        return CodegenType.OTHER;
    }

    @Override
    public DummyOptionsBuilder optionsBuilder() {
        return new DummyOptionsBuilder(this);
    }

    void setGreeting(String greeting) {
        this.greeting = greeting;
    }

    public void setJoy(boolean joy) {
        this.joy = joy;
    }

    public void setMagicNumber(int magic) {
        this.magicNumber = magic;
    }

    @Override
    public void processOpts() {
        super.processOpts();
        additionalProperties.put("greeting", greeting);
        additionalProperties.put("joy", joy);
        additionalProperties.put("magicNumber", magicNumber);
        apiTemplateFiles.clear();
        apiTemplateFiles.put("server/dummy.mustache", ".java");
    }
}
