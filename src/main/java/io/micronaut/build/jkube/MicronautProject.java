package io.micronaut.build.jkube;

import io.micronaut.build.MicronautRuntime;
import io.micronaut.build.services.DockerService;
import org.eclipse.jkube.kit.common.JavaProject;

import java.io.File;
import java.util.List;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
public class MicronautProject extends JavaProject {

    private final DockerService dockerService;
    private final MicronautRuntime micronautRuntime;
    private final List<String> args;
    private final File targetDirectory;

//    public MicronautProject(String name, String groupId, String artifactId, String version, File outputDirectory, File baseDirectory, File buildDirectory, Properties properties, List<String> compileClassPathElements, List<Dependency> dependencies, List<Dependency> dependenciesWithTransitive, List<Plugin> plugins, String site, String description, String organizationName, String documentationUrl, String buildFinalName, File artifact, File localRepositoryBaseDirectory, String packaging, String issueManagementSystem, String issueManagementUrl, String scmUrl, String scmTag) {
//        super(name, groupId, artifactId, version, outputDirectory, baseDirectory, buildDirectory, properties, compileClassPathElements, dependencies, dependenciesWithTransitive, plugins, site, description, organizationName, documentationUrl, buildFinalName, artifact, localRepositoryBaseDirectory, packaging, issueManagementSystem, issueManagementUrl, scmUrl, scmTag);
//    }

    public MicronautProject(JavaProject p, DockerService dockerService, MicronautRuntime micronautRuntime, List<String> args, File targetDirectory) {
        super(p.getName(), p.getGroupId(), p.getArtifactId(), p.getVersion(), p.getOutputDirectory(), p.getBaseDirectory(),
                p.getBuildDirectory(), p.getProperties(), p.getCompileClassPathElements(), p.getDependencies(),
                p.getDependenciesWithTransitive(), p.getPlugins(), p.getSite(), p.getDescription(), p.getOrganizationName(),
                p.getDocumentationUrl(), p.getBuildFinalName(), p.getArtifact(), p.getLocalRepositoryBaseDirectory(),
                p.getPackaging(), p.getIssueManagementSystem(), p.getIssueManagementUrl(), p.getScmUrl(), p.getScmTag());

        this.dockerService = dockerService;
        this.micronautRuntime = micronautRuntime;
        this.args = args;
        this.targetDirectory = targetDirectory;
    }

    public DockerService getDockerService() {
        return dockerService;
    }

    public MicronautRuntime getMicronautRuntime() {
        return micronautRuntime;
    }

    public List<String> getArgs() {
        return args;
    }

    public File getTargetDirectory() {
        return targetDirectory;
    }
}
