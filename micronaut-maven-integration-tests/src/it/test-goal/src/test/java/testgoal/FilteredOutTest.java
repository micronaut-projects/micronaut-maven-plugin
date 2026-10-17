package testgoal;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.fail;

class FilteredOutTest {

    @Test
    void neverRuns() {
        fail("-Dtest leaves this class out");
    }
}
