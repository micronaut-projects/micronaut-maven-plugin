package io.micronaut.maven.info;

import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.io.TempDir;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;

class GitInfoGeneratorTest {

    @Test
    void returnsUnavailableOutsideGitRepository(@TempDir Path tempDir) {
        GitInfoGenerator.Result result = new GitInfoGenerator(tempDir, true, false, false).generate(null);

        assertTrue(result.properties().isEmpty());
        assertFalse(result.message().isBlank());
    }

    @Test
    void generatesConservativeGitProperties(@TempDir Path tempDir) throws IOException, InterruptedException {
        git(tempDir, "init");
        git(tempDir, "config", "user.email", "dev@example.com");
        git(tempDir, "config", "user.name", "Dev User");
        git(tempDir, "remote", "add", "origin", "https://example.com/private/repo.git");
        Files.writeString(tempDir.resolve("README.md"), "test");
        git(tempDir, "add", "README.md");
        git(tempDir, "commit", "-m", "Initial commit");

        Map<String, String> properties = new GitInfoGenerator(tempDir, true, false, false).generate(null).properties();

        assertEquals(gitOutput(tempDir, "rev-parse", "HEAD"), properties.get("git.commit.id"));
        assertEquals(gitOutput(tempDir, "rev-parse", "--short", "HEAD"), properties.get("git.commit.id.abbrev"));
        assertEquals("false", properties.get("git.dirty"));
        assertTrue(properties.containsKey("git.commit.time"));
        assertFalse(properties.containsKey("git.remote.origin.url"));
        assertFalse(properties.containsKey("git.build.user.name"));
        assertFalse(properties.containsKey("git.build.user.email"));
    }

    @Test
    void includesSensitiveFieldsOnlyWhenEnabled(@TempDir Path tempDir) throws IOException, InterruptedException {
        git(tempDir, "init");
        git(tempDir, "config", "user.email", "dev@example.com");
        git(tempDir, "config", "user.name", "Dev User");
        git(tempDir, "remote", "add", "origin", "https://example.com/private/repo.git");
        Files.writeString(tempDir.resolve("README.md"), "test");
        git(tempDir, "add", "README.md");
        git(tempDir, "commit", "-m", "Initial commit");

        Map<String, String> properties = new GitInfoGenerator(tempDir, false, true, true).generate(null).properties();

        assertEquals("https://example.com/private/repo.git", properties.get("git.remote.origin.url"));
        assertEquals("Dev User", properties.get("git.build.user.name"));
        assertEquals("dev@example.com", properties.get("git.build.user.email"));
        assertFalse(properties.containsKey("git.dirty"));
    }

    @Test
    void ignoresAdditionalGitPropertiesWithBlankKeys(@TempDir Path tempDir) throws IOException, InterruptedException {
        git(tempDir, "init");
        git(tempDir, "config", "user.email", "dev@example.com");
        git(tempDir, "config", "user.name", "Dev User");
        Files.writeString(tempDir.resolve("README.md"), "test");
        git(tempDir, "add", "README.md");
        git(tempDir, "commit", "-m", "Initial commit");

        Map<String, String> properties = new GitInfoGenerator(tempDir, false, false, false).generate(Map.of(" ", "ignored", "git.valid", "included")).properties();

        assertFalse(properties.containsKey(" "));
        assertEquals("included", properties.get("git.valid"));
    }

    private void git(Path directory, String... args) throws IOException, InterruptedException {
        Process process = new ProcessBuilder(command(args))
            .directory(directory.toFile())
            .redirectErrorStream(true)
            .start();
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8);
        assertEquals(0, process.waitFor(), output);
    }

    private String gitOutput(Path directory, String... args) throws IOException, InterruptedException {
        Process process = new ProcessBuilder(command(args))
            .directory(directory.toFile())
            .redirectErrorStream(true)
            .start();
        String output = new String(process.getInputStream().readAllBytes(), StandardCharsets.UTF_8).trim();
        assertEquals(0, process.waitFor(), output);
        return output;
    }

    private String[] command(String... args) {
        String[] command = new String[args.length + 1];
        command[0] = "git";
        System.arraycopy(args, 0, command, 1, args.length);
        return command;
    }
}
