package io.micronaut.build.examples;

import io.micronaut.test.extensions.junit5.annotation.MicronautTest;
import jakarta.inject.Inject;
import org.junit.jupiter.api.DisplayName;
import org.junit.jupiter.api.Test;

import java.util.List;
import java.util.UUID;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertNotNull;

@MicronautTest
class DemoTest {

    @Inject
    BookRepository bookRepository;

    @Test
    @DisplayName("A MySQL test container is required to run this test")
    void testItWorks() {
        Book book = new Book();
        book.setTitle("Yet Another Book " + UUID.randomUUID());
        Book saved = bookRepository.save(book);
        assertNotNull(saved.getId());
        List<Book> books = bookRepository.findAll();

        //Deliberately make it fail
        assertEquals(20, books.size());
    }

}
