plugins {
    kotlin("jvm") version "1.9.20"
}

kotlin {
    jvmToolchain(11)
}

group = "org.example"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
    // https://mvnrepository.com/artifact/org.antlr/antlr4
    implementation("org.antlr:antlr4:4.13.1")
    // https://mvnrepository.com/artifact/org.jetbrains.kotlin/kotlin-reflect
    runtimeOnly("org.jetbrains.kotlin:kotlin-reflect:1.9.20")


}

tasks.test {
    useJUnitPlatform()
}

sourceSets {
    main {
        kotlin {
            srcDirs("src/main/gen", "src/main/granny")
        }
    }
    main {
        java {
            srcDirs("src/main/gen", "src/main/granny")
        }
    }
}