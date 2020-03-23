package example

import io.micronaut.runtime.Micronaut
import groovy.transform.CompileStatic
import org.apache.commons.text.WordUtils

@CompileStatic
class Application {
    static void main(String[] args) {
        println WordUtils.capitalize("hola")
        Micronaut.run(Application)
    }
}