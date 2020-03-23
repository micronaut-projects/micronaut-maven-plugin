package io.micronaut.build.examples.greeter;

//import org.apache.commons.text.WordUtils;

import org.apache.commons.text.WordUtils;

/**
 * TODO: javadoc
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 1.0.0
 */
public class Greeter {

    public String greet(String name) {
        return "HELLO " + WordUtils.capitalize(name);
    }
}
