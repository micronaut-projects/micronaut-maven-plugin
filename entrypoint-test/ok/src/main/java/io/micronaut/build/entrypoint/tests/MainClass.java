package io.micronaut.build.entrypoint.tests;

import org.apache.commons.text.WordUtils;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
public class MainClass {

    public static void main(String[] args) {
        System.out.println(WordUtils.capitalize("I'm the main class!"));
    }
}
