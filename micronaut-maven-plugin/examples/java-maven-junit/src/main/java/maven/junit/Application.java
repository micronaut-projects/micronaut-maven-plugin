package maven.junit;

import io.micronaut.runtime.Micronaut;
import org.apache.commons.text.WordUtils;

public class Application {

    public static void main(String[] args) {
//        System.out.println(WordUtils.capitalize("asdasd"));
        Micronaut.run(Application.class);
    }
}