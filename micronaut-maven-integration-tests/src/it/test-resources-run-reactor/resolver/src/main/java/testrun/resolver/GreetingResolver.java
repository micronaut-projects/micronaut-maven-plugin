package testrun.resolver;

import io.micronaut.testresources.core.TestResourcesResolver;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;

/**
 * Resolves the greeting in the Test Resources service: a resolver that needs no Docker.
 */
public final class GreetingResolver implements TestResourcesResolver {
    private static final String PROPERTY = "greeting.message";

    @Override
    public List<String> getResolvableProperties(Map<String, Collection<String>> propertyEntries, Map<String, Object> testResourcesConfig) {
        return List.of(PROPERTY);
    }

    @Override
    public Optional<String> resolve(String propertyName, Map<String, Object> properties, Map<String, Object> testResourcesConfig) {
        return PROPERTY.equals(propertyName) ? Optional.of("Hello from the test resources service") : Optional.empty();
    }
}
