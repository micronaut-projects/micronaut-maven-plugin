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
package io.micronaut.maven.openapi;

import io.micronaut.openapi.generator.AbstractMicronautJavaCodegen;
import io.micronaut.openapi.generator.GeneratorOptionsBuilder;
import io.micronaut.openapi.generator.MicronautCodeGenerator;
import io.micronaut.openapi.generator.MicronautCodeGeneratorBuilder;
import org.apache.commons.lang3.StringUtils;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugins.annotations.LifecyclePhase;
import org.apache.maven.plugins.annotations.Mojo;
import org.apache.maven.plugins.annotations.Parameter;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Locale;
import java.util.Map;

/**
 * A generic OpenAPI mojo that will be used for configuring custom Micronaut OpenAPI generator extensions.
 */
@Mojo(name = OpenApiGenericMojo.MOJO_NAME, defaultPhase = LifecyclePhase.GENERATE_SOURCES)
public class OpenApiGenericMojo extends AbstractOpenApiMojo {

    public static final String MOJO_NAME = "generate-openapi-generic";
    public static final String CONFIGURATION_PROPERTIES = MICRONAUT_OPENAPI_PREFIX + ".generator.properties";

    /**
     * The classname of the generator to be used for code generation.
     *
     * <p>The generator must property overwrite the {@link AbstractMicronautJavaCodegen#optionsBuilder()} method
     * and the builder should have setters or withers for the properties to be used in maven configuration.</p>
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".generator.builder.classname")
    protected String generatorClassName;

    /**
     * The configuration properties that will be passed on to the custom generator options builder.
     *
     * <p>Any configuration parameters with key {@code micronaut.openapi.generator.properties.[PROPERTY_NAME]}
     * will be passed on to the generator options builder. String, integer and boolean value types are supported
     * for additional properties.</p>
     */
    @Parameter(property = CONFIGURATION_PROPERTIES)
    protected Map<String, String> properties;

    @Override
    protected boolean isEnabled() {
        return generatorClassName != null;
    }

    @Override
    protected void configureBuilder(MicronautCodeGeneratorBuilder builder) throws MojoExecutionException {
        MicronautCodeGenerator<? extends GeneratorOptionsBuilder> generator = instantiateGenerator();

        try {
            builder.forCodeGenerator(generator, config -> {
                for (Map.Entry<String, String> entry : properties.entrySet()) {
                    String name = entry.getKey().substring(CONFIGURATION_PROPERTIES.length() + 1);
                    String value = entry.getValue();
                    invokeMethod(name, config, value);
                }
            });
        } catch (OpenAPIInvocationException e) {
            throw new MojoExecutionException(e);
        }
    }

    private MicronautCodeGenerator<? extends GeneratorOptionsBuilder> instantiateGenerator() {
        MicronautCodeGenerator<? extends GeneratorOptionsBuilder> generator;
        try {
            generator = (MicronautCodeGenerator<? extends GeneratorOptionsBuilder>) this.getClass()
                .getClassLoader()
                .loadClass(generatorClassName)
                .getDeclaredConstructor()
                .newInstance();
        } catch (InstantiationException | IllegalAccessException | InvocationTargetException | NoSuchMethodException | ClassNotFoundException e) {
            throw new RuntimeException(e);
        }
        return generator;
    }

    private static void invokeMethod(String name, GeneratorOptionsBuilder builder, String value) {
        try {
            String witherName = "with" + StringUtils.capitalize(name);
            String setterName = "set" + StringUtils.capitalize(name);
            Class<? extends GeneratorOptionsBuilder> builderClazz = builder.getClass();
            var methods = builderClazz.getDeclaredMethods();
            for (Method method : methods) {
                if (invokeIfMatches(name, builder, value, witherName, setterName, method)) {
                    return;
                }
            }
            throw new OpenAPIInvocationException("Unable to find a method on builder " + builderClazz + " with name '" + name + "' which accepts argument '" + value + "'");
        } catch (IllegalAccessException | InvocationTargetException ex) {
            throw new OpenAPIInvocationException(ex);
        }
    }

    private static boolean invokeIfMatches(String name, GeneratorOptionsBuilder builder, String value, String witherName, String setterName, Method method) throws IllegalAccessException, InvocationTargetException {
        var methodName = method.getName();
        if ((methodName.equals(name) || methodName.equals(witherName) || methodName.equals(setterName)) && method.getParameterCount() == 1) {
            Class<?> parameterType = method.getParameterTypes()[0];
            if (parameterType.equals(String.class)) {
                method.invoke(builder, value);
                return true;
            } else if (parameterType.equals(Boolean.TYPE)) {
                var coerced = value.toLowerCase(Locale.ENGLISH);
                if ("true".equals(coerced) || "false".equals(coerced)) {
                    method.invoke(builder, Boolean.parseBoolean(coerced));
                    return true;
                }
            } else if (parameterType.equals(Integer.TYPE) && (value.matches("[0-9]+"))) {
                method.invoke(builder, Integer.parseInt(value));
                return true;

            }
        }
        return false;
    }

    /**
     * Exception to be thrown when OpenAPI generator configuration fails.
     */
    static class OpenAPIInvocationException extends RuntimeException {

        public OpenAPIInvocationException(String message) {
            super(message);
        }

        public OpenAPIInvocationException(Throwable throwable) {
            super(throwable);
        }
    }
}
