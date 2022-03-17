/*
 * Copyright 2003-2021 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package io.micronaut.build.aot;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

abstract class Constants {
    static final List<String> TYPES_TO_CHECK = Collections.unmodifiableList(Arrays.asList(
            "io.reactivex.Observable",
            "reactor.core.publisher.Flux",
            "kotlinx.coroutines.flow.Flow",
            "io.reactivex.rxjava3.core.Flowable",
            "io.reactivex.rxjava3.core.Observable",
            "io.reactivex.Single",
            "reactor.core.publisher.Mono",
            "io.reactivex.Maybe",
            "io.reactivex.rxjava3.core.Single",
            "io.reactivex.rxjava3.core.Maybe",
            "io.reactivex.Completable",
            "io.reactivex.rxjava3.core.Completable",
            "io.methvin.watchservice.MacOSXListeningWatchService",
            "io.micronaut.core.async.publisher.CompletableFuturePublisher",
            "io.micronaut.core.async.publisher.Publishers.JustPublisher",
            "io.micronaut.core.async.subscriber.Completable"
    ));

    static final List<String> SERVICE_TYPES = Collections.unmodifiableList(Arrays.asList(
            "io.micronaut.context.env.PropertySourceLoader",
            "io.micronaut.inject.BeanConfiguration",
            "io.micronaut.inject.BeanDefinitionReference",
            "io.micronaut.http.HttpRequestFactory",
            "io.micronaut.http.HttpResponseFactory",
            "io.micronaut.core.beans.BeanIntrospectionReference"
    ));

    static final String MICRONAUT_AOT_GROUP_ID = "io.micronaut.aot";
    static final String MICRONAUT_AOT_ARTIFACT_ID_PREFIX = "micronaut-aot-";
    static final String MICRONAUT_AOT_MAIN_CLASS = "io.micronaut.aot.cli.Main";

}
