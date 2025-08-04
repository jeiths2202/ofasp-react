#!/bin/bash

# SMED System Build Script
# Builds the complete SMED parsing, deployment, and web display system

echo "=== SMED System Build Script ==="
echo "Building SMED Map Parsing and Deployment System"
echo

# Configuration
JAVA_SRC_DIR="/home/aspuser/app/server/java_src"
BUILD_OUTPUT_DIR="/home/aspuser/app/server/smed_build_output"
JAR_OUTPUT_DIR="/home/aspuser/app/server/java_jars"

# Create output directories
echo "--- Creating build directories ---"
mkdir -p "$BUILD_OUTPUT_DIR"
mkdir -p "$JAR_OUTPUT_DIR"
mkdir -p "/home/aspuser/app/volume/TEST_DISK/TEST_LIB"

# Change to source directory
cd "$JAVA_SRC_DIR"

# Check if Maven is available
if command -v mvn &> /dev/null; then
    echo "--- Using Maven for build ---"
    
    # Clean previous build
    echo "Cleaning previous build..."
    mvn clean
    
    # Compile and package
    echo "Compiling and packaging..."
    mvn compile package
    
    # Copy built JAR to output directory
    if [ -f "target/smed-system-1.0.0-jar-with-dependencies.jar" ]; then
        cp "target/smed-system-1.0.0-jar-with-dependencies.jar" "$JAR_OUTPUT_DIR/smed-system.jar"
        echo "✓ Maven build successful"
        echo "JAR file: $JAR_OUTPUT_DIR/smed-system.jar"
    else
        echo "✗ Maven build failed - JAR not found"
        exit 1
    fi
    
else
    echo "--- Using manual compilation (Maven not available) ---"
    
    # Create classpath for Jackson libraries
    JACKSON_VERSION="2.15.2"
    JACKSON_CORE_URL="https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-core/$JACKSON_VERSION/jackson-core-$JACKSON_VERSION.jar"
    JACKSON_DATABIND_URL="https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-databind/$JACKSON_VERSION/jackson-databind-$JACKSON_VERSION.jar"
    JACKSON_ANNOTATIONS_URL="https://repo1.maven.org/maven2/com/fasterxml/jackson/core/jackson-annotations/$JACKSON_VERSION/jackson-annotations-$JACKSON_VERSION.jar"
    
    LIB_DIR="$BUILD_OUTPUT_DIR/lib"
    mkdir -p "$LIB_DIR"
    
    # Download Jackson libraries if not present
    echo "Downloading Jackson libraries..."
    
    if [ ! -f "$LIB_DIR/jackson-core-$JACKSON_VERSION.jar" ]; then
        curl -L -o "$LIB_DIR/jackson-core-$JACKSON_VERSION.jar" "$JACKSON_CORE_URL"
    fi
    
    if [ ! -f "$LIB_DIR/jackson-databind-$JACKSON_VERSION.jar" ]; then
        curl -L -o "$LIB_DIR/jackson-databind-$JACKSON_VERSION.jar" "$JACKSON_DATABIND_URL"
    fi
    
    if [ ! -f "$LIB_DIR/jackson-annotations-$JACKSON_VERSION.jar" ]; then
        curl -L -o "$LIB_DIR/jackson-annotations-$JACKSON_VERSION.jar" "$JACKSON_ANNOTATIONS_URL"
    fi
    
    # Build classpath
    CLASSPATH="$LIB_DIR/jackson-core-$JACKSON_VERSION.jar:$LIB_DIR/jackson-databind-$JACKSON_VERSION.jar:$LIB_DIR/jackson-annotations-$JACKSON_VERSION.jar"
    
    echo "Compiling Java sources..."
    
    # Create output directory for compiled classes
    mkdir -p "$BUILD_OUTPUT_DIR/classes"
    
    # Compile all Java files
    find . -name "*.java" -type f > "$BUILD_OUTPUT_DIR/sources.txt"
    
    javac -cp "$CLASSPATH" -d "$BUILD_OUTPUT_DIR/classes" -encoding UTF-8 @"$BUILD_OUTPUT_DIR/sources.txt"
    
    if [ $? -eq 0 ]; then
        echo "✓ Compilation successful"
        
        # Create JAR file
        echo "Creating JAR file..."
        cd "$BUILD_OUTPUT_DIR/classes"
        
        # Create manifest
        cat > "$BUILD_OUTPUT_DIR/MANIFEST.MF" << EOF
Manifest-Version: 1.0
Main-Class: com.openasp.smed.SMEDTestRunner
Class-Path: lib/jackson-core-$JACKSON_VERSION.jar lib/jackson-databind-$JACKSON_VERSION.jar lib/jackson-annotations-$JACKSON_VERSION.jar

EOF
        
        # Create JAR
        jar cfm "$JAR_OUTPUT_DIR/smed-system.jar" "$BUILD_OUTPUT_DIR/MANIFEST.MF" .
        
        # Copy libraries
        cp -r "$LIB_DIR" "$JAR_OUTPUT_DIR/"
        
        echo "✓ JAR creation successful"
        echo "JAR file: $JAR_OUTPUT_DIR/smed-system.jar"
        
    else
        echo "✗ Compilation failed"
        exit 1
    fi
fi

echo
echo "--- Build Summary ---"
echo "Source directory: $JAVA_SRC_DIR"
echo "Output JAR: $JAR_OUTPUT_DIR/smed-system.jar"
echo "Build output: $BUILD_OUTPUT_DIR"

# List generated files
echo
echo "--- Generated Files ---"
ls -la "$JAR_OUTPUT_DIR/"

echo
echo "=== SMED System Build Completed Successfully ==="
echo
echo "To run the test system:"
echo "cd $JAR_OUTPUT_DIR"
echo "java -jar smed-system.jar"
echo