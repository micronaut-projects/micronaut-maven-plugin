package io.micronaut.build.entrypoint.test;

import java.io.InputStream;
import java.lang.reflect.Method;
import java.net.URL;
import java.net.URLClassLoader;
import java.nio.file.Files;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.nio.file.StandardCopyOption;
import java.util.ArrayList;
import java.util.List;
import java.util.jar.JarFile;

public class GeneratedEntrypoint {

    public static void main(String[] args) throws Exception {
        GeneratedEntrypoint entrypoint = new GeneratedEntrypoint();
        entrypoint.run(args);
    }

    public void run(String[] args) throws Exception {
        System.out.println("I'm the generated entrypoint!");

        URL jarUrl = getClass().getProtectionDomain().getCodeSource().getLocation();
        JarFile file = new JarFile(Paths.get(jarUrl.toURI()).toFile());
        List<URL> urls = new ArrayList<>();
        urls.add(jarUrl);
        for (String dependency : file.getManifest().getMainAttributes().getValue("Class-Path").split(" ")) {
            URL url = getClass().getClassLoader().getResource(dependency);
            if (url != null) {
                Path path = Files.createTempFile(dependency.substring(dependency.indexOf("/") + 1, dependency.lastIndexOf(".")), ".jar");
                path.toFile().deleteOnExit();
                try (InputStream is = url.openStream()) {
                    Files.copy(is, path, StandardCopyOption.REPLACE_EXISTING);
                }
                urls.add(path.toUri().toURL());
//                urls.add(url);
            }
        }
        URLClassLoader classLoader = new URLClassLoader(urls.toArray(new URL[0]), ClassLoader.getSystemClassLoader().getParent());
        Thread.currentThread().setContextClassLoader(classLoader);

        String entrypoint = file.getManifest().getMainAttributes().getValue("Micronaut-Entrypoint");
        Class<?> entrypointClass = classLoader.loadClass(entrypoint);
        Method main = entrypointClass.getDeclaredMethod("main", String[].class);
        main.setAccessible(true);
        main.invoke(null, new Object[] { args });

//        MainClass.main(args);
    }

}
