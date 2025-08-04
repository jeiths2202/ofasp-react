#!/bin/bash

# OpenASP Java Libraries Setup Script
# 필요한 Java 라이브러리들을 다운로드하고 설치합니다.

echo "=== OpenASP Java Libraries Setup ==="
echo "Setting up required libraries for MAIN001 and other Java programs..."

# 라이브러리 디렉토리 생성
JAVA_LIBS_DIR="/home/aspuser/app/server/java_jars"
mkdir -p $JAVA_LIBS_DIR
cd $JAVA_LIBS_DIR

echo "Current directory: $(pwd)"

# 1. SLF4J (Simple Logging Facade for Java)
echo "Downloading SLF4J libraries..."
if [ ! -f "slf4j-api-2.0.9.jar" ]; then
    wget -q https://repo1.maven.org/maven2/org/slf4j/slf4j-api/2.0.9/slf4j-api-2.0.9.jar
    echo "✓ slf4j-api-2.0.9.jar downloaded"
else
    echo "✓ slf4j-api-2.0.9.jar already exists"
fi

if [ ! -f "slf4j-simple-2.0.9.jar" ]; then
    wget -q https://repo1.maven.org/maven2/org/slf4j/slf4j-simple/2.0.9/slf4j-simple-2.0.9.jar
    echo "✓ slf4j-simple-2.0.9.jar downloaded"
else
    echo "✓ slf4j-simple-2.0.9.jar already exists"
fi

# 2. Spring Framework Core
echo "Downloading Spring Framework libraries..."
if [ ! -f "spring-core-6.0.13.jar" ]; then
    wget -q https://repo1.maven.org/maven2/org/springframework/spring-core/6.0.13/spring-core-6.0.13.jar
    echo "✓ spring-core-6.0.13.jar downloaded"
else
    echo "✓ spring-core-6.0.13.jar already exists"
fi

if [ ! -f "spring-context-6.0.13.jar" ]; then
    wget -q https://repo1.maven.org/maven2/org/springframework/spring-context/6.0.13/spring-context-6.0.13.jar
    echo "✓ spring-context-6.0.13.jar downloaded"
else
    echo "✓ spring-context-6.0.13.jar already exists"
fi

if [ ! -f "spring-beans-6.0.13.jar" ]; then
    wget -q https://repo1.maven.org/maven2/org/springframework/spring-beans/6.0.13/spring-beans-6.0.13.jar
    echo "✓ spring-beans-6.0.13.jar downloaded"
else
    echo "✓ spring-beans-6.0.13.jar already exists"
fi

if [ ! -f "spring-expression-6.0.13.jar" ]; then
    wget -q https://repo1.maven.org/maven2/org/springframework/spring-expression/6.0.13/spring-expression-6.0.13.jar
    echo "✓ spring-expression-6.0.13.jar downloaded"
else
    echo "✓ spring-expression-6.0.13.jar already exists"
fi

# 3. Jackson JSON Processing (JSONResponse 대체용)
echo "Downloading Jackson JSON libraries..."
if [ ! -f "jackson-core-2.15.2.jar" ]; then
    wget -q https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-core/2.15.2/jackson-core-2.15.2.jar
    echo "✓ jackson-core-2.15.2.jar downloaded"
else
    echo "✓ jackson-core-2.15.2.jar already exists"
fi

if [ ! -f "jackson-databind-2.15.2.jar" ]; then
    wget -q https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-databind/2.15.2/jackson-databind-2.15.2.jar
    echo "✓ jackson-databind-2.15.2.jar downloaded"
else
    echo "✓ jackson-databind-2.15.2.jar already exists"
fi

if [ ! -f "jackson-annotations-2.15.2.jar" ]; then
    wget -q https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-annotations/2.15.2/jackson-annotations-2.15.2.jar
    echo "✓ jackson-annotations-2.15.2.jar downloaded"
else
    echo "✓ jackson-annotations-2.15.2.jar already exists"
fi

# 4. 현재 라이브러리 목록 표시
echo ""
echo "=== Installed Libraries ==="
ls -la *.jar | awk '{print "✓ " $9 " (" $5 " bytes)"}'

# 5. Classpath 설정 정보
echo ""
echo "=== Classpath Setup ==="
echo "To compile Java programs, use:"
echo "javac -encoding UTF-8 -cp \"$JAVA_LIBS_DIR/*\" YourJavaFile.java"
echo ""
echo "To run Java programs, use:"
echo "java -cp \"$JAVA_LIBS_DIR/*:.\" YourClassName"

# 6. 환경변수 설정 제안
echo ""
echo "=== Environment Variables ==="
echo "Add to your ~/.bashrc:"
echo "export JAVA_LIBS=\"$JAVA_LIBS_DIR\""
echo "export CLASSPATH=\"\$JAVA_LIBS/*:\$CLASSPATH\""

echo ""
echo "✅ Library setup completed!"
echo "Total JAR files: $(ls -1 *.jar | wc -l)"