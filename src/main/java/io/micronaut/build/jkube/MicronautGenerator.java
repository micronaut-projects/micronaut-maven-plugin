package io.micronaut.build.jkube;

import io.micronaut.build.MicronautRuntime;
import org.eclipse.jkube.generator.api.GeneratorContext;
import org.eclipse.jkube.generator.javaexec.JavaExecGenerator;
import org.eclipse.jkube.kit.common.AssemblyFileSet;
import org.eclipse.jkube.kit.common.util.JKubeProjectUtil;
import org.eclipse.jkube.kit.config.image.ImageConfiguration;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
public class MicronautGenerator extends JavaExecGenerator {

    public MicronautGenerator(GeneratorContext context) {
        super(context, "micronaut");
    }

    @Override
    public boolean isApplicable(List<ImageConfiguration> configs) {
        return shouldAddGeneratedImageConfiguration(configs)
                && JKubeProjectUtil.hasPlugin(getProject(), "io.micronaut.build", "micronaut-maven-plugin");
    }

    @Override
    public List<ImageConfiguration> customize(List<ImageConfiguration> configs, boolean prePackagePhase) {
        MicronautProject project = (MicronautProject) getProject();
        MicronautRuntime runtime = project.getMicronautRuntime();
        getContext().getLogger().info("Micronaut runtime: %s", runtime);

        Optional<ImageConfiguration> imageConfiguration = Optional.empty();
        if (configs.size() > 0) {
            if (configs.size() > 1) {
                getContext().getLogger().warn("Only one image configuration should be defined. The first one will be used");
            }
            imageConfiguration = Optional.of(configs.get(0));
        }

        return Collections.singletonList(project.getDockerService().createImageConfiguration(imageConfiguration.orElse(ImageConfiguration.builder().build()), runtime, project.getArgs(), project.getTargetDirectory()));
    }

    @Override
    public List<AssemblyFileSet> addAdditionalFiles() {
        List<AssemblyFileSet> fileSets = new ArrayList<>(3);
        fileSets.add(createFileSet("target/layers/libs", "/home/app/libs", "0644"));
        fileSets.add(createFileSet("target/layers/resources", "/home/app/resources", "0644"));
        fileSets.add(createFileSet("target/layers", "/home/app", "0644"));
        return fileSets;
    }

    @Override
    //TODO
    protected List<String> extractPorts() {
        return Collections.singletonList("8080");
    }
}
