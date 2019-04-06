import org.jetbrains.kotlin.gradle.tasks.KotlinCompile

plugins {
    kotlin("jvm") version "1.3.11"
    application
}

application {
    mainClassName = "compiler.Compiler"
}

version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    compile(kotlin("stdlib-jdk8"))
    
    // https://mvnrepository.com/artifact/org.ow2.asm/asm
    compile(group="org.ow2.asm", name="asm", version="7.1")

}

tasks.withType<KotlinCompile> {
    kotlinOptions.jvmTarget = "1.8"
}