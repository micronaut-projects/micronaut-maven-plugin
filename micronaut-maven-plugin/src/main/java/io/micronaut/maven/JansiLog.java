/*
 * Copyright 2017-2023 original authors
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
package io.micronaut.maven;

import org.apache.maven.plugin.logging.Log;

import static org.fusesource.jansi.Ansi.ansi;

/**
 * A {@link Log} implementation that uses Jansi to colorize the output.
 *
 * @author Álvaro Sánchez-Mariscal
 * @since 4.0.0
 */
public class JansiLog implements Log {

    private final Log delegate;

    public JansiLog(Log delegate) {
        this.delegate = delegate;
    }

    @Override
    public boolean isDebugEnabled() {
        return delegate.isDebugEnabled();
    }

    @Override
    public void debug(CharSequence content) {
        delegate.debug(fmt(content));
    }

    @Override
    public void debug(CharSequence content, Throwable error) {
        delegate.debug(fmt(content), error);
    }

    @Override
    public void debug(Throwable error) {
        delegate.debug(error);
    }

    @Override
    public boolean isInfoEnabled() {
        return delegate.isInfoEnabled();
    }

    @Override
    public void info(CharSequence content) {
        delegate.info(fmt(content));
    }

    @Override
    public void info(CharSequence content, Throwable error) {
        delegate.info(fmt(content), error);
    }

    @Override
    public void info(Throwable error) {
        delegate.info(error);
    }

    @Override
    public boolean isWarnEnabled() {
        return delegate.isWarnEnabled();
    }

    @Override
    public void warn(CharSequence content) {
        delegate.warn(fmt(content));
    }

    @Override
    public void warn(CharSequence content, Throwable error) {
        delegate.warn(fmt(content), error);
    }

    @Override
    public void warn(Throwable error) {
        delegate.warn(error);
    }

    @Override
    public boolean isErrorEnabled() {
        return delegate.isErrorEnabled();
    }

    @Override
    public void error(CharSequence content) {
        delegate.error(fmt(content));
    }

    @Override
    public void error(CharSequence content, Throwable error) {
        delegate.error(fmt(content), error);
    }

    @Override
    public void error(Throwable error) {
        delegate.error(error);
    }

    private String fmt(CharSequence s) {
        return ansi().fgYellow().a(s).reset().toString();
    }

}
