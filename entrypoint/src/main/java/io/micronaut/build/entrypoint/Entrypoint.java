package io.micronaut.build.entrypoint;

import java.io.IOException;
import java.io.InputStream;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.*;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.Attributes;
import java.util.jar.JarFile;

public class Entrypoint {

    private static final String MICRONAUT_ENTRYPOINT = "Micronaut-Entrypoint";
    private static final String CLASS_PATH = "Class-Path";

    public static void main(String[] args) throws Exception {
        Entrypoint entrypoint = new Entrypoint();
        entrypoint.run(args);
    }

    public void run(String[] args) throws Exception {
        URL jarUrl = getClass().getProtectionDomain().getCodeSource().getLocation();
        JarFile file = new JarFile(Paths.get(jarUrl.toURI()).toFile());
        Attributes manifest = file.getManifest().getMainAttributes();
        URLClassLoader classLoader = createClassLoader(manifest, jarUrl);
        invokeEntrypoint(args, manifest, classLoader);
    }

    private void invokeEntrypoint(String[] args, Attributes manifest, URLClassLoader classLoader) throws ClassNotFoundException, NoSuchMethodException, IllegalAccessException, InvocationTargetException {
        String entrypoint = manifest.getValue(MICRONAUT_ENTRYPOINT);
        Class<?> entrypointClass = Class.forName(entrypoint, false, classLoader);
        Method main = entrypointClass.getDeclaredMethod("main", String[].class);
        main.setAccessible(true);
        main.invoke(null, new Object[] {args});
    }

    private URLClassLoader createClassLoader(Attributes manifest, URL jarUrl) throws IOException {
        List<URL> urls = new ArrayList<>();
        urls.add(jarUrl);
        copyDependencies(manifest, urls);
        URLClassLoader classLoader = URLClassLoader.newInstance(urls.toArray(new URL[0]), ClassLoader.getSystemClassLoader().getParent());
        Thread.currentThread().setContextClassLoader(classLoader);
        return classLoader;
    }

    private void copyDependencies(Attributes manifest, List<URL> urls) throws IOException {
        for (String dependency : manifest.getValue(CLASS_PATH).split(" ")) {
            URL url = getClass().getClassLoader().getResource(dependency);
            if (url != null) {
                Path path = Files.createTempFile(dependency.substring(dependency.indexOf("/") + 1, dependency.lastIndexOf(".")), ".jar");
                path.toFile().deleteOnExit();
                try (InputStream is = url.openStream()) {
                    Files.copy(is, path, StandardCopyOption.REPLACE_EXISTING);
                }
                urls.add(path.toUri().toURL());
            }
        }
    }

}
