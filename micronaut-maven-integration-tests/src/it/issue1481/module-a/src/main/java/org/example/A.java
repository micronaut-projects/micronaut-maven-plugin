package org.example;

import jakarta.inject.Singleton;

@Singleton
public class A {
    public void start() {
        System.out.println("Start");
    }

    public void stop() {
        System.out.println("Stop");
    }
}
