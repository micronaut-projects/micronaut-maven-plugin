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

import io.micronaut.maven.AbstractMicronautMojo;
import io.micronaut.openapi.generator.MicronautCodeGeneratorBuilder;
import io.micronaut.openapi.generator.MicronautCodeGeneratorEntryPoint;
import io.micronaut.openapi.generator.MicronautCodeGeneratorOptionsBuilder;
import io.micronaut.openapi.generator.SerializationLibraryKind;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.MojoFailureException;
import org.apache.maven.plugins.annotations.Parameter;
import org.apache.maven.project.MavenProject;

import java.io.File;
import java.util.List;
import java.util.Locale;
import java.util.Map;

/**
 * Base class for OpenAPI generator mojos. This provides the common
 * parameters for all generators and the invoker logic. Subclasses
 * must implement the {@link #isEnabled()} and {@link #configureBuilder(MicronautCodeGeneratorBuilder)}
 * methods.
 */
public abstract class AbstractOpenApiMojo extends AbstractMicronautMojo {
    static final String MICRONAUT_OPENAPI_PREFIX = "micronaut.openapi";
    static final String IO_MICRONAUT_OPENAPI_PREFIX = "io.micronaut.openapi";

    /**
     * The OpenAPI specification file path relative to the project's root path.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".definition", defaultValue = IO_MICRONAUT_OPENAPI_PREFIX + ".invoker", required = true)
    protected File definitionFile;

    /**
     * The name of the package that can be used for various classes required for invocation.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".invoker.package.name", defaultValue = IO_MICRONAUT_OPENAPI_PREFIX + ".invoker", required = true)
    protected String invokerPackageName;

    /**
     * The package name for the APIs (controller interfaces).
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".api.package.name", defaultValue = IO_MICRONAUT_OPENAPI_PREFIX + ".api", required = true)
    protected String apiPackageName;

    /**
     * The package name for the model classes.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".model.package.name", defaultValue = IO_MICRONAUT_OPENAPI_PREFIX + ".model", required = true)
    protected String modelPackageName;

    /**
     * Whether to generate validation annotations for models and APIs.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".use.bean.validation", defaultValue = "true", required = true)
    protected boolean useBeanValidation;

    /**
     * Flag to indicate whether to use the utils.OneOfImplementorAdditionalData related logic.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".use.one.of.interfaces", defaultValue = "true", required = true)
    protected boolean useOneOfInterfaces;

    /**
     * Whether to use {@link java.util.Optional} for non-required model properties and API parameters.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".use.optional", defaultValue = "false", required = true)
    protected boolean useOptional;

    /**
     * Whether to use reactor types for operation responses.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".use.reactive", defaultValue = "true", required = true)
    protected boolean useReactive;

    /**
     * Configure the serialization library.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".serialization.framework", defaultValue = "MICRONAUT_SERDE_JACKSON", required = true)
    protected String serializationFramework;

    /**
     * If true, the generated operation return types will be wrapped in HttpResponse.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".always.use.generate.http.response", defaultValue = "false", required = true)
    protected boolean alwaysUseGenerateHttpResponse;

    /**
     * Wrap the operations response in HttpResponse object where non-200 HTTP status codes or additional headers are defined.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".generate.http.response.where.required", defaultValue = "false", required = true)
    protected boolean generateHttpResponseWhereRequired;

    /**
     * If set to true, generated code will be fully compatible with KSP, but not 100% with KAPT.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".ksp", defaultValue = "false", required = true)
    protected boolean ksp;

    /**
     * Configure the date-time format.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".date.time.format", defaultValue = "ZONED_DATETIME", required = true)
    protected String dateTimeFormat;

    /**
     * Comma-separated values of output kinds to generate. The values are defined by the
     * {@link MicronautCodeGeneratorEntryPoint.OutputKind} enum.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".outputs", required = true, defaultValue = "apis,models,supporting_files")
    protected List<String> outputKinds;

    /**
     * The output directory to which all the sources will be generated.
     */
    @Parameter(defaultValue = "${project.build.directory}/generated-sources/openapi", required = true)
    protected File outputDirectory;

    /**
     * Define parameter mappings that allow using custom types for parameter binding.
     * See {@link ParameterMapping} for details.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".parameterMappings")
    protected List<ParameterMapping> parameterMappings;

    /**
     * Define parameter mappings that allow using custom types for parameter binding.
     * See {@link ResponseBodyMapping} for details.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".responseBodyMappings")
    protected List<ResponseBodyMapping> responseBodyMappings;

    /**
     * Add the schema mappings.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".schemaMapping")
    protected Map<String, String> schemaMapping;

    /**
     * Add the import mappings.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".importMapping")
    protected Map<String, String> importMapping;

    /**
     * Add the name mappings.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".nameMapping")
    protected Map<String, String> nameMapping;

    /**
     * Add the type mappings.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".typeMapping")
    protected Map<String, String> typeMapping;

    /**
     * Add the enum name mappings.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".enumNameMapping")
    protected Map<String, String> enumNameMapping;

    /**
     * Add the model name mappings.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".modelNameMapping")
    protected Map<String, String> modelNameMapping;

    /**
     * Add the inline schema name mappings.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".inlineSchemaNameMapping")
    protected Map<String, String> inlineSchemaNameMapping;

    /**
     * Add the inline schema options.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".inlineSchemaOption")
    protected Map<String, String> inlineSchemaOption;

    /**
     * Add the OpenAPI normalizer options.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".openapiNormalizer")
    protected Map<String, String> openapiNormalizer;

    /**
     * Set the api name prefix.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".apiNamePrefix")
    protected String apiNamePrefix;

    /**
     * Set the api name suffix.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".apiNameSuffix")
    protected String apiNameSuffix;

    /**
     * Set the model name prefix.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".modelNamePrefix")
    protected String modelNamePrefix;

    /**
     * Set the model name suffix.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".modelNameSuffix")
    protected String modelNameSuffix;

    /**
     * Allows specifying the language of the generated code.
     *
     * @since 4.3.0
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".lang", defaultValue = "java")
    protected String lang;

    /**
     * If set to true, the generated enums check enum value with ignoring case.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".useEnumCaseInsensitive", defaultValue = "false")
    protected boolean useEnumCaseInsensitive;

    /**
     * Additional annotations for enum type (class level annotations).
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".additionalEnumTypeAnnotations")
    protected List<String> additionalEnumTypeAnnotations;

    /**
     * Additional annotations for model type (class level annotations).
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".additionalModelTypeAnnotations")
    protected List<String> additionalModelTypeAnnotations;

    /**
     * Additional annotations for oneOf interfaces (class level annotations).
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".additionalOneOfTypeAnnotations")
    protected List<String> additionalOneOfTypeAnnotations;

    /**
     * Additional generator properties.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".additionalProperties")
    protected Map<String, Object> additionalProperties;

    /**
     * If set to true, controller and client method will be generated with openAPI annotations.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".generateSwaggerAnnotations", defaultValue = "false")
    protected boolean generateSwaggerAnnotations;

    /**
     * Set the implicit headers flag.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".implicitHeaders", defaultValue = "false")
    protected boolean implicitHeaders;

    /**
     * Set the implicit headers regex.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".implicitHeadersRegex")
    protected String implicitHeadersRegex;

    /**
     * Flag to indicate whether to use the "jakarta" or "javax" package.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".useJakartaEe", defaultValue = "true")
    protected boolean useJakartaEe = true;

    /**
     * Sort method arguments to place required parameters before optional parameters.
     * Default: true
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".sortParamsByRequiredFlag", defaultValue = "true")
    protected boolean sortParamsByRequiredFlag = true;

    /**
     * Skip examples defined in operations to avoid out of memory errors.
     * Default: false
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".skipOperationExample")
    protected boolean skipOperationExample;

    /**
     * Skip sorting operations.
     * Default: false
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".skipSortingOperations")
    protected boolean skipSortingOperations;

    /**
     * Character to use as a delimiter for the prefix. Default: '_'
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".removeOperationIdPrefixDelimiter", defaultValue = "_")
    protected String removeOperationIdPrefixDelimiter = "_";

    /**
     * Count of delimiter for the prefix. Use -1 for last. Default: 1
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".removeOperationIdPrefixCount", defaultValue = "1")
    protected int removeOperationIdPrefixCount = 1;

    /**
     * Sort model properties to place required parameters before optional parameters.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".sortModelPropertiesByRequiredFlag", defaultValue = "true")
    protected boolean sortModelPropertiesByRequiredFlag = true;

    /**
     * Whether to ensure parameter names are unique in an operation (rename parameters that are not).
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".ensureUniqueParams", defaultValue = "true")
    protected boolean ensureUniqueParams = true;

    /**
     * boolean, toggles whether Unicode identifiers are allowed in names or not, default is false.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".allowUnicodeIdentifiers")
    protected boolean allowUnicodeIdentifiers;

    /**
     * Add form or body parameters to the beginning of the parameter list.
     */
    @Parameter(property = MICRONAUT_OPENAPI_PREFIX + ".prependFormOrBodyParameters")
    protected boolean prependFormOrBodyParameters;

    @Parameter(defaultValue = "${project}", readonly = true)
    protected MavenProject project;

    /**
     * Determines if this mojo must be executed.
     *
     * @return true if the mojo is enabled
     */
    protected abstract boolean isEnabled();

    /**
     * Configures the OpenAPI generator. When this method is called,
     * common properties shared by all generators have already been
     * configured, so this method should only take care of configuring
     * the generator specific parameters.
     *
     * @param builder the generator configuration builder
     */
    protected abstract void configureBuilder(MicronautCodeGeneratorBuilder builder) throws MojoExecutionException;

    @Override
    public final void execute() throws MojoExecutionException, MojoFailureException {
        if (!isEnabled()) {
            getLog().debug(this.getClass().getSimpleName() + " is disabled");
            return;
        }

        project.addCompileSourceRoot(outputDirectory.getAbsolutePath());
        var builder = MicronautCodeGeneratorEntryPoint.builder()
            .withDefinitionFile(definitionFile.toURI())
            .withOutputDirectory(outputDirectory)
            .withOutputs(
                outputKinds.stream().map(String::toUpperCase).map(MicronautCodeGeneratorEntryPoint.OutputKind::valueOf).toList().toArray(new MicronautCodeGeneratorEntryPoint.OutputKind[0])
            )
            .withOptions(options -> options
                .withLang(MicronautCodeGeneratorOptionsBuilder.GeneratorLanguage.valueOf(lang.toUpperCase(Locale.ENGLISH)))
                .withApiPackage(apiPackageName)
                .withModelPackage(modelPackageName)
                .withInvokerPackage(invokerPackageName)
                .withBeanValidation(useBeanValidation)
                .withUseOneOfInterfaces(useOneOfInterfaces)
                .withOptional(useOptional)
                .withReactive(useReactive)
                .withSerializationLibrary(SerializationLibraryKind.valueOf(serializationFramework.toUpperCase(Locale.ENGLISH)))
                .withGenerateHttpResponseAlways(alwaysUseGenerateHttpResponse)
                .withGenerateHttpResponseWhereRequired(generateHttpResponseWhereRequired)
                .withDateTimeFormat(MicronautCodeGeneratorOptionsBuilder.DateTimeFormat.valueOf(dateTimeFormat.toUpperCase(Locale.ENGLISH)))
                .withParameterMappings(parameterMappings.stream()
                    .map(mapping -> new io.micronaut.openapi.generator.ParameterMapping(
                        mapping.getName(),
                        io.micronaut.openapi.generator.ParameterMapping.ParameterLocation.valueOf(
                            mapping.getLocation().name()
                        ),
                        mapping.getMappedType(),
                        mapping.getMappedName(),
                        mapping.isValidated()
                    ))
                    .toList()
                )
                .withResponseBodyMappings(responseBodyMappings.stream()
                    .map(mapping -> new io.micronaut.openapi.generator.ResponseBodyMapping(
                        mapping.getHeaderName(),
                        mapping.getMappedBodyType(),
                        mapping.isListWrapper(),
                        mapping.isValidated()
                    ))
                    .toList()
                )
                .withSchemaMapping(schemaMapping)
                .withImportMapping(importMapping)
                .withNameMapping(nameMapping)
                .withTypeMapping(typeMapping)
                .withEnumNameMapping(enumNameMapping)
                .withModelNameMapping(modelNameMapping)
                .withInlineSchemaNameMapping(inlineSchemaNameMapping)
                .withInlineSchemaOption(inlineSchemaOption)
                .withOpenapiNormalizer(openapiNormalizer)
                .withApiNamePrefix(apiNamePrefix != null ? apiNamePrefix : "")
                .withApiNameSuffix(apiNameSuffix != null ? apiNameSuffix : "")
                .withModelNamePrefix(modelNamePrefix != null ? modelNamePrefix : "")
                .withModelNameSuffix(modelNameSuffix != null ? modelNameSuffix : "")
                .withGenerateSwaggerAnnotations(generateSwaggerAnnotations)
                .withImplicitHeaders(implicitHeaders)
                .withImplicitHeadersRegex(implicitHeadersRegex != null ? implicitHeadersRegex : "")
                .withUseEnumCaseInsensitive(useEnumCaseInsensitive)
                .withAdditionalEnumTypeAnnotations(additionalEnumTypeAnnotations)
                .withAdditionalModelTypeAnnotations(additionalModelTypeAnnotations)
                .withAdditionalOneOfTypeAnnotations(additionalOneOfTypeAnnotations)
                .withAdditionalProperties(additionalProperties)

                .withUseJakartaEe(useJakartaEe)
                .withSortParamsByRequiredFlag(sortParamsByRequiredFlag)
                .withSkipOperationExample(skipOperationExample)
                .withSkipSortingOperations(skipSortingOperations)
                .withRemoveOperationIdPrefixDelimiter(removeOperationIdPrefixDelimiter)
                .withRemoveOperationIdPrefixCount(removeOperationIdPrefixCount)
                .withSortModelPropertiesByRequiredFlag(sortModelPropertiesByRequiredFlag)
                .withEnsureUniqueParams(ensureUniqueParams)
                .withAllowUnicodeIdentifiers(allowUnicodeIdentifiers)
                .withPrependFormOrBodyParameters(prependFormOrBodyParameters)
            );
        configureBuilder(builder);
        builder.build().generate();
    }
}
