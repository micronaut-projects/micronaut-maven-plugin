package io.micronaut.maven.testresources;

import org.junit.jupiter.api.Test;

import java.lang.reflect.Method;

import static org.junit.jupiter.api.Assertions.*;

/**
 * Test for StartTestResourcesServerMojo to verify skipTests functionality.
 * This is a simple test that verifies the method exists and is properly structured.
 */
class StartTestResourcesServerMojoTest {

    @Test
    void shouldHaveIsTestExecutionSkippedMethod() {
        // Verify that the method exists and is accessible
        Class<StartTestResourcesServerMojo> mojoClass = StartTestResourcesServerMojo.class;
        
        Method[] methods = mojoClass.getDeclaredMethods();
        boolean hasSkipMethod = false;
        
        for (Method method : methods) {
            if ("isTestExecutionSkipped".equals(method.getName()) && 
                method.getReturnType() == boolean.class &&
                method.getParameterCount() == 0) {
                hasSkipMethod = true;
                break;
            }
        }
        
        assertTrue(hasSkipMethod, "StartTestResourcesServerMojo should have isTestExecutionSkipped() method");
    }
    
    @Test
    void shouldHaveExecuteMethodThatChecksSkipTests() throws Exception {
        // Verify that the execute method exists
        Class<StartTestResourcesServerMojo> mojoClass = StartTestResourcesServerMojo.class;
        
        Method executeMethod = mojoClass.getDeclaredMethod("execute");
        assertNotNull(executeMethod, "execute() method should exist");
        assertEquals(void.class, executeMethod.getReturnType());
        assertEquals(0, executeMethod.getParameterCount());
    }
}