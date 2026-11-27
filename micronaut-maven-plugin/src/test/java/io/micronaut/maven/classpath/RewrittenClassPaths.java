package io.micronaut.maven.classpath;

import java.nio.file.Path;
import java.util.Map;

/**
 * Creates rewritten class paths for the tests of other packages.
 */
public final class RewrittenClassPaths {

    private RewrittenClassPaths() {
    }

    /**
     * @param mapping the absolute class path entries and their rewritten copies
     * @return a rewritten class path
     */
    public static RewrittenClassPath of(Map<Path, Path> mapping) {
        return new RewrittenClassPath(mapping);
    }
}
