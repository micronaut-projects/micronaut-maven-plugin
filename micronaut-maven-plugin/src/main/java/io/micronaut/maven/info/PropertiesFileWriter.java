/*
 * Copyright 2017-2026 original authors
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.micronaut.maven.info;

import java.io.IOException;
import java.nio.charset.StandardCharsets;
import java.nio.file.Files;
import java.nio.file.Path;
import java.util.Map;
import java.util.TreeMap;

/**
 * Writes sorted Java properties files without timestamp comments.
 *
 * @author Micronaut Authors
 * @since 5.0.1
 */
final class PropertiesFileWriter {

    private PropertiesFileWriter() {
    }

    static void write(Path outputFile, Map<String, String> properties) throws IOException {
        Path parent = outputFile.getParent();
        if (parent != null) {
            Files.createDirectories(parent);
        }
        var content = new StringBuilder();
        new TreeMap<>(properties).forEach((key, value) -> content
            .append(escape(key, true))
            .append('=')
            .append(escape(value, false))
            .append('\n'));
        Files.writeString(outputFile, content.toString(), StandardCharsets.UTF_8);
    }

    static String escape(String value, boolean key) {
        var escaped = new StringBuilder(value.length());
        for (int i = 0; i < value.length(); i++) {
            char c = value.charAt(i);
            switch (c) {
                case '\\' -> escaped.append("\\\\");
                case '\t' -> escaped.append("\\t");
                case '\n' -> escaped.append("\\n");
                case '\r' -> escaped.append("\\r");
                case '\f' -> escaped.append("\\f");
                case '=', ':', '#', '!' -> escaped.append('\\').append(c);
                case ' ' -> {
                    if (key || i == 0) {
                        escaped.append("\\ ");
                    } else {
                        escaped.append(c);
                    }
                }
                default -> escaped.append(c);
            }
        }
        return escaped.toString();
    }
}
