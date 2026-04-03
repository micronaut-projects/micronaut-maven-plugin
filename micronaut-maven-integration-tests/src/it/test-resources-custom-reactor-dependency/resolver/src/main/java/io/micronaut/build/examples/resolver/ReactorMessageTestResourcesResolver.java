package io.micronaut.build.examples.resolver;

import io.micronaut.testresources.core.TestResourcesResolver;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Optional;

public final class ReactorMessageTestResourcesResolver implements TestResourcesResolver {
    static final String PROPERTY = "test.resource.from.reactor";
    static final String VALUE = "hello-from-reactor";

    @Override
    public List<String> getResolvableProperties(Map<String, Collection<String>> propertyEntries,
                                                Map<String, Object> testResourcesConfig) {
        return List.of(PROPERTY);
    }

    @Override
    public Optional<String> resolve(String propertyName,
                                    Map<String, Object> properties,
                                    Map<String, Object> testResourcesConfig) {
        if (PROPERTY.equals(propertyName)) {
            return Optional.of(VALUE);
        }
        return Optional.empty();
    }
}
