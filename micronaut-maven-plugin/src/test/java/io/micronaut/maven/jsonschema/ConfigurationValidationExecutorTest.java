package io.micronaut.maven.jsonschema;

import io.micronaut.jsonschema.configuration.validator.DependencyInjectionError;
import io.micronaut.jsonschema.configuration.validator.cli.DependencyInjectionConfigurationValidator;
import org.junit.jupiter.api.Test;
import org.mockito.MockedStatic;

import java.lang.reflect.Method;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Set;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.mockito.Mockito.mock;
import static org.mockito.Mockito.mockStatic;
import static org.mockito.Mockito.when;

class ConfigurationValidationExecutorTest {

    @Test
    void validateDependencyInjectionUsesSuppressionAwareValidatorWhenAvailable() throws Exception {
        DependencyInjectionConfigurationValidator validator = mock(DependencyInjectionConfigurationValidator.class);
        DependencyInjectionError error = dependencyError("a.Root", "a.Bean", "a.Bean.field");
        Set<DependencyInjectionError> expected = Set.of(error);
        when(validator.validate()).thenReturn(expected);

        try (MockedStatic<DependencyInjectionConfigurationValidator> mocked = mockStatic(DependencyInjectionConfigurationValidator.class)) {
            mocked.when(() -> DependencyInjectionConfigurationValidator.forClasspath("cp", List.of("test"), false, List.of("a.*")))
                .thenReturn(validator);

            Set<DependencyInjectionError> actual = invokeValidateDependencyInjection("cp", List.of("test"), false, List.of("a.*"));

            assertEquals(expected, actual);
        }
    }

    @Test
    void applyLegacySuppressionsFiltersExactAndWildcardPatterns() throws Exception {
        DependencyInjectionError keep = dependencyError("com.example.Keep", "com.example.Other", "x.y");
        DependencyInjectionError suppressByRoot = dependencyError("com.example.SuppressedRoot", "com.example.Bean", "x.y");
        DependencyInjectionError suppressByBean = dependencyError("com.example.Root", "com.example.ignore.MyBean", "x.y");

        Set<DependencyInjectionError> errors = new LinkedHashSet<>();
        errors.add(keep);
        errors.add(suppressByRoot);
        errors.add(suppressByBean);

        Set<DependencyInjectionError> filtered = invokeApplyLegacySuppressions(
            errors,
            List.of("com.example.SuppressedRoot", "com.example.ignore.*")
        );

        assertEquals(1, filtered.size());
        assertTrue(filtered.contains(keep));
    }

    @Test
    void applyLegacySuppressionsReturnsOriginalSetWhenPatternsEmpty() throws Exception {
        Set<DependencyInjectionError> errors = Set.of(dependencyError("a.Root", "a.Bean", "a.Bean.field"));

        Set<DependencyInjectionError> filtered = invokeApplyLegacySuppressions(errors, List.of());

        assertEquals(errors, filtered);
    }

    @Test
    void wildcardToRegexEscapesRegexCharacters() throws Exception {
        String regex = invokeWildcardToRegex("com.example.$Service*");

        assertNotNull(regex);
        assertTrue("com.example.$ServiceImpl".matches(regex));
        assertFalse("com.other.ServiceImpl".matches(regex));
    }

    @SuppressWarnings("unchecked")
    private static Set<DependencyInjectionError> invokeValidateDependencyInjection(String classpath,
                                                                                   List<String> environments,
                                                                                   boolean deduceEnvironments,
                                                                                   List<String> suppressInjectErrors) throws Exception {
        Method method = ConfigurationValidationExecutor.class.getDeclaredMethod(
            "validateDependencyInjection",
            String.class,
            List.class,
            boolean.class,
            List.class
        );
        method.setAccessible(true);
        return (Set<DependencyInjectionError>) method.invoke(null, classpath, environments, deduceEnvironments, suppressInjectErrors);
    }

    @SuppressWarnings("unchecked")
    private static Set<DependencyInjectionError> invokeApplyLegacySuppressions(Set<DependencyInjectionError> errors,
                                                                                List<String> suppressInjectErrors) throws Exception {
        Method method = ConfigurationValidationExecutor.class.getDeclaredMethod(
            "applyLegacySuppressions",
            Set.class,
            List.class
        );
        method.setAccessible(true);
        return (Set<DependencyInjectionError>) method.invoke(null, errors, suppressInjectErrors);
    }

    private static String invokeWildcardToRegex(String wildcardPattern) throws Exception {
        Method method = ConfigurationValidationExecutor.class.getDeclaredMethod("wildcardToRegex", String.class);
        method.setAccessible(true);
        return (String) method.invoke(null, wildcardPattern);
    }

    private static DependencyInjectionError dependencyError(String rootBean, String bean, String injectionPoint) {
        DependencyInjectionError error = mock(DependencyInjectionError.class);
        when(error.rootBean()).thenReturn(rootBean);
        when(error.bean()).thenReturn(bean);
        when(error.injectionPoint()).thenReturn(injectionPoint);
        return error;
    }
}
