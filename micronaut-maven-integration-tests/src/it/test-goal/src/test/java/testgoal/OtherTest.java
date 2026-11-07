package testgoal;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.fail;

class OtherTest {

    @Test
    void greets() {
        assertEquals("Hello Maven", new Greeter().greet("Maven"));
    }

    @Test
    void counts() {
        assertEquals(5, "Maven".length());
    }

    @Test
    void filteredOut() {
        fail("-Dtest=OtherTest#greets+counts leaves this test out");
    }
}
