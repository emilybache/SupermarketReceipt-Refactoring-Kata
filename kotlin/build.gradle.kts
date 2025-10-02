plugins {
    kotlin("jvm") version "2.2.0"
}

group = "org.sammancoaching"
version = "1.0-SNAPSHOT"

repositories {
    mavenCentral()
}

dependencies {
    testImplementation(kotlin("test"))
    testImplementation("org.mockito.kotlin:mockito-kotlin:5.4.0")
    testImplementation("com.approvaltests:approvaltests:25.4.3")
}

tasks.test {
    useJUnitPlatform()
}
kotlin {
    jvmToolchain(22)
}