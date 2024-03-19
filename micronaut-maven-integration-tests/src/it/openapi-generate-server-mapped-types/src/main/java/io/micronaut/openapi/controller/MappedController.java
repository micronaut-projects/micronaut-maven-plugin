package io.micronaut.openapi.controller;


import io.micronaut.http.annotation.*;
import reactor.core.publisher.Mono;
import io.micronaut.openapi.dated.DatedResponse;
import io.micronaut.openapi.filter.MyFilter;
import io.micronaut.data.model.Page;
import io.micronaut.data.model.Pageable;
import java.time.ZonedDateTime;
import jakarta.annotation.Generated;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.swagger.v3.oas.annotations.security.SecurityRequirement;
import io.micronaut.openapi.api.MappedApi;
import java.time.ZonedDateTime;


@Controller
public class MappedController implements MappedApi {

    @Override
    public Mono<DatedResponse<String>> getDatedSimpleModel() {
        return Mono.just(DatedResponse.of("Hello").withLastModified(ZonedDateTime.now()));
    }

    @Override
    public Mono<Page<String>> getPaginatedSimpleModel(Pageable pageable) {
        return Mono.just(Page.of(List.of(), pageable, 0));
    }

    @Override
    public Mono<String> sendIgnoredHeader() {
        return Mono.just("Hello");
    }

    @Override
    public Mono<String> sendMappedParameter(MyFilter myFilter) {
        return Mono.just(myFilter.toString());
    }

    @Override
    public Mono<String> sendPageQuery(Pageable pageable) {
        return Mono.just(String.valueOf(pageable));
    }

}