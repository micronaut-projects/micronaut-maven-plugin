package io.micronaut.build.services;

import org.apache.maven.execution.MavenSession;
import org.apache.maven.plugin.MojoExecutionException;
import org.apache.maven.plugin.logging.Log;
import org.apache.maven.plugin.logging.SystemStreamLog;
import org.apache.maven.project.*;
import org.apache.maven.shared.invoker.*;
import org.eclipse.aether.RepositorySystemSession;
import org.eclipse.aether.graph.Dependency;
import org.eclipse.aether.graph.DependencyFilter;
import org.eclipse.aether.util.filter.DependencyFilterUtils;

import javax.inject.Inject;
import javax.inject.Singleton;
import java.io.File;
import java.nio.file.Path;
import java.util.*;
import java.util.concurrent.atomic.AtomicReference;
import java.util.function.Function;
import java.util.stream.Collectors;
import java.util.stream.Stream;

/**
 * Provides methods to compile a Maven project
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.1
 */
@Singleton
public class CompilerService {

    private static final String JAVA = "java";
    private static final String GROOVY = "groovy";
    private static final String KOTLIN = "kotlin";

    public static final String MAVEN_COMPILER_PLUGIN = "org.apache.maven.plugins:maven-compiler-plugin";
    public static final String MAVEN_JAR_PLUGIN = "org.apache.maven.plugins:maven-jar-plugin";
    public static final String MAVEN_RESOURCES_PLUGIN = "org.apache.maven.plugins:maven-resources-plugin";
    public static final String GMAVEN_PLUS_PLUGIN = "org.codehaus.gmavenplus:gmavenplus-plugin";
    public static final String KOTLIN_MAVEN_PLUGIN = "org.jetbrains.kotlin:kotlin-maven-plugin";


    /**
     * @see <a href="https://maven.apache.org/ref/3.6.3/maven-core/lifecycles.html#default_Lifecycle">default Lifecycle</a>
     */
    private static final List<String> PHASES_AFTER_COMPILE = Arrays.asList(
            "compile",
            "process-classes",
            "generate-test-sources",
            "process-test-sources",
            "generate-test-resources",
            "process-test-resources",
            "test-compile",
            "process-test-classes",
            "test",
            "prepare-package",
            "package",
            "pre-integration-test",
            "integration-test",
            "post-integration-test",
            "verify",
            "install",
            "deploy");

    private final Log log;
    private final Map<String, Path> sourceDirectories;
    private final MavenProject mavenProject;
    private final MavenSession mavenSession;
    private final ExecutorService executorService;
    private final ProjectDependenciesResolver resolver;

    @SuppressWarnings("CdiInjectionPointsInspection")
    @Inject
    public CompilerService(MavenProject mavenProject, MavenSession mavenSession, ExecutorService executorService,
                           ProjectDependenciesResolver resolver) {
        this.resolver = resolver;
        this.log = new SystemStreamLog();
        this.mavenProject = mavenProject;
        this.mavenSession = mavenSession;
        this.executorService = executorService;
        this.sourceDirectories = resolveSourceDirectories();
    }

    public boolean needsCompilation() {
        return mavenSession.getGoals().stream().noneMatch(PHASES_AFTER_COMPILE::contains);
    }

    public Optional<Long> compileProject(boolean copyResources) {
        Long lastCompilation = null;
        if (log.isDebugEnabled()) {
            log.debug("Compiling the project");
        }
        try {
            if (sourceDirectories.containsKey(GROOVY)) {
                executorService.executeGoal(GMAVEN_PLUS_PLUGIN, "addSources");
                executorService.executeGoal(GMAVEN_PLUS_PLUGIN, "generateStubs");
                if (copyResources) {
                    executorService.executeGoal(MAVEN_RESOURCES_PLUGIN, "resources");
                }
                executorService.executeGoal(MAVEN_COMPILER_PLUGIN, "compile");
                executorService.executeGoal(GMAVEN_PLUS_PLUGIN, "compile");
                executorService.executeGoal(GMAVEN_PLUS_PLUGIN, "removeStubs");
                lastCompilation = System.currentTimeMillis();
            }
            if (sourceDirectories.containsKey(KOTLIN)) {
                executorService.executeGoal(KOTLIN_MAVEN_PLUGIN, "kapt");
                if (copyResources) {
                    executorService.executeGoal(MAVEN_RESOURCES_PLUGIN, "resources");
                }
                executorService.executeGoal(KOTLIN_MAVEN_PLUGIN, "compile");
                executorService.executeGoal(MAVEN_COMPILER_PLUGIN, "compile#java-compile");
                lastCompilation = System.currentTimeMillis();
            }
            if (sourceDirectories.containsKey(JAVA)) {
                if (copyResources) {
                    executorService.executeGoal(MAVEN_RESOURCES_PLUGIN, "resources");
                }
                executorService.executeGoal(MAVEN_COMPILER_PLUGIN, "compile");
                lastCompilation = System.currentTimeMillis();
            }
        } catch (MojoExecutionException e) {
            if (log.isErrorEnabled()) {
                log.error("Error while compiling the project: ", e);
            }
        }
        return Optional.ofNullable(lastCompilation);
    }

    public Map<String, Path> resolveSourceDirectories() {
        if (log.isDebugEnabled()) {
            log.debug("Resolving source directories...");
        }
        AtomicReference<String> lang = new AtomicReference<>();
        Map<String, Path> sourceDirectories = Stream.of(JAVA, GROOVY, KOTLIN)
                .peek(lang::set)
                .map(l -> new File(mavenProject.getBasedir(), "src/main/" + l))
                .filter(File::exists)
                .peek(f -> {
                    if (log.isDebugEnabled()) {
                        log.debug("Found source: " + f.getPath());
                    }
                })
                .map(File::toPath)
                .collect(Collectors.toMap(path -> lang.get(), Function.identity()));
        if (sourceDirectories.isEmpty()) {
            throw new IllegalStateException("Source folders not found for neither Java/Groovy/Kotlin");
        }
        return sourceDirectories;
    }

    public List<Dependency> resolveDependencies(String... scopes) {
        try {
            DependencyFilter filter = DependencyFilterUtils.classpathFilter(scopes);
            RepositorySystemSession session = mavenSession.getRepositorySession();
            DependencyResolutionRequest dependencyResolutionRequest = new DefaultDependencyResolutionRequest(mavenProject, session);
            dependencyResolutionRequest.setResolutionFilter(filter);
            DependencyResolutionResult result = resolver.resolve(dependencyResolutionRequest);
            return result.getDependencies();
        } catch (org.apache.maven.project.DependencyResolutionException e) {
            if (log.isWarnEnabled()) {
                log.warn("Error while trying to resolve dependencies for the current project", e);
            }
            return Collections.emptyList();
        }
    }

    public String buildClasspath(List<Dependency> dependencies) {
        Comparator<Dependency> byGroupId = Comparator.comparing(d -> d.getArtifact().getGroupId());
        Comparator<Dependency> byArtifactId = Comparator.comparing(d -> d.getArtifact().getArtifactId());
        return dependencies.stream()
                .sorted(byGroupId.thenComparing(byArtifactId))
                .map(dependency -> dependency.getArtifact().getFile().getAbsolutePath())
                .collect(Collectors.joining(File.pathSeparator));
    }

    public InvocationResult packageProject() throws MavenInvocationException {
        InvocationRequest request = new DefaultInvocationRequest();
        request.setPomFile(mavenProject.getFile());
        request.setGoals(Collections.singletonList(MAVEN_JAR_PLUGIN + ":jar"));
        request.setBatchMode(true);
        request.setQuiet(true);
        Invoker invoker = new DefaultInvoker();
        return invoker.execute(request);
    }
}
