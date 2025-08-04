package com.openasp.smed;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.text.SimpleDateFormat;

/**
 * SMED Map Deployment System
 * 
 * Requirements:
 * - Parameters: DISK, Library selection
 * - Output filename: SMED맵명.json
 * - Deploy JSON files to specified location
 */
public class SMEDDeploymentSystem {
    
    private static final String DEFAULT_DISK = "DISK01";
    private static final String DEFAULT_LIBRARY = "TESTLIB";
    private static final String BASE_PATH = "/home/aspuser/app/volume";
    
    // 1차 목표 사양: 배포 위치 /home/aspuser/app/volume/{DISK}/{LIBRARY}/
    
    private SMEDMapParser parser;
    private String targetDisk;
    private String targetLibrary;
    private String deploymentPath;
    
    public SMEDDeploymentSystem() {
        this.parser = new SMEDMapParser();
        this.targetDisk = DEFAULT_DISK;
        this.targetLibrary = DEFAULT_LIBRARY;
        this.updateDeploymentPath();
    }
    
    public SMEDDeploymentSystem(String disk, String library) {
        this.parser = new SMEDMapParser();
        this.targetDisk = disk != null ? disk : DEFAULT_DISK;
        this.targetLibrary = library != null ? library : DEFAULT_LIBRARY;
        this.updateDeploymentPath();
    }
    
    private void updateDeploymentPath() {
        this.deploymentPath = Paths.get(BASE_PATH, targetDisk, targetLibrary).toString();
    }
    
    /**
     * Deploy SMED map to specified DISK and Library
     * 
     * @param smedFilePath Path to source SMED map file
     * @return DeploymentResult containing deployment status and information
     */
    public DeploymentResult deploySMEDMap(String smedFilePath) {
        DeploymentResult result = new DeploymentResult();
        result.setSourceFile(smedFilePath);
        result.setTargetDisk(targetDisk);
        result.setTargetLibrary(targetLibrary);
        result.setDeploymentTime(new Date());
        
        try {
            // Step 1: Parse SMED file
            System.out.println("Step 1: Parsing SMED file: " + smedFilePath);
            SMEDMapParser.ParseResult parseResult = parser.parseSMEDFile(smedFilePath);
            
            if (!parseResult.isSuccess()) {
                result.setSuccess(false);
                result.setErrorMessage("Failed to parse SMED file: " + parseResult.getErrorMessage());
                return result;
            }
            
            // Step 2: Generate output filename (1차 목표 사양: SMED맵명.json)
            String mapName = parseResult.getMapData().getMapName();
            String outputFileName = mapName + ".json";
            result.setOutputFileName(outputFileName);
            
            System.out.println("Generated filename format: " + outputFileName + " (SMED맵명.json)");
            
            // Step 3: Create deployment directory
            System.out.println("Step 2: Creating deployment directory: " + deploymentPath);
            if (!createDeploymentDirectory()) {
                result.setSuccess(false);
                result.setErrorMessage("Failed to create deployment directory: " + deploymentPath);
                return result;
            }
            
            // Step 4: Deploy JSON file
            String targetFilePath = Paths.get(deploymentPath, outputFileName).toString();
            System.out.println("Step 3: Deploying JSON to: " + targetFilePath);
            
            if (!writeJSONFile(targetFilePath, parseResult.getJsonContent())) {
                result.setSuccess(false);
                result.setErrorMessage("Failed to write JSON file: " + targetFilePath);
                return result;
            }
            
            // Step 5: Verify deployment
            System.out.println("Step 4: Verifying deployment");
            if (!verifyDeployment(targetFilePath, parseResult.getMapData())) {
                result.setSuccess(false);
                result.setErrorMessage("Deployment verification failed");
                return result;
            }
            
            // Success
            result.setSuccess(true);
            result.setTargetFilePath(targetFilePath);
            result.setDeployedFields(parseResult.getMapData().getParsedFields());
            
            System.out.println("=== SMED Deployment Successful ===");
            System.out.println("Source: " + smedFilePath);
            System.out.println("Target: " + targetFilePath);
            System.out.println("Map Name: " + mapName);
            System.out.println("Fields: " + result.getDeployedFields());
            System.out.println("Disk: " + targetDisk);
            System.out.println("Library: " + targetLibrary);
            
        } catch (Exception e) {
            result.setSuccess(false);
            result.setErrorMessage("Exception during deployment: " + e.getMessage());
            e.printStackTrace();
        }
        
        return result;
    }
    
    /**
     * Create deployment directory structure
     */
    private boolean createDeploymentDirectory() {
        try {
            Path deployPath = Paths.get(deploymentPath);
            
            if (!Files.exists(deployPath)) {
                Files.createDirectories(deployPath);
                System.out.println("Created deployment directory: " + deploymentPath);
            } else {
                System.out.println("Deployment directory already exists: " + deploymentPath);
            }
            
            return true;
            
        } catch (IOException e) {
            System.err.println("Failed to create deployment directory: " + e.getMessage());
            return false;
        }
    }
    
    /**
     * Write JSON content to target file
     */
    private boolean writeJSONFile(String targetFilePath, String jsonContent) {
        try {
            Path targetPath = Paths.get(targetFilePath);
            Files.write(targetPath, jsonContent.getBytes("UTF-8"), 
                       StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING);
            
            System.out.println("Successfully wrote JSON file: " + targetFilePath);
            System.out.println("File size: " + Files.size(targetPath) + " bytes");
            
            return true;
            
        } catch (IOException e) {
            System.err.println("Failed to write JSON file: " + e.getMessage());
            return false;
        }
    }
    
    /**
     * Verify successful deployment
     */
    private boolean verifyDeployment(String targetFilePath, SMEDMapParser.SMEDMapData originalData) {
        try {
            Path targetPath = Paths.get(targetFilePath);
            
            // Check file exists
            if (!Files.exists(targetPath)) {
                System.err.println("Verification failed: Deployed file does not exist");
                return false;
            }
            
            // Check file is readable
            if (!Files.isReadable(targetPath)) {
                System.err.println("Verification failed: Deployed file is not readable");
                return false;
            }
            
            // Check file size
            long fileSize = Files.size(targetPath);
            if (fileSize == 0) {
                System.err.println("Verification failed: Deployed file is empty");
                return false;
            }
            
            // Read and validate JSON content
            String deployedContent = new String(Files.readAllBytes(targetPath), "UTF-8");
            if (!deployedContent.contains(originalData.getMapName())) {
                System.err.println("Verification failed: Map name not found in deployed file");
                return false;
            }
            
            System.out.println("Deployment verification successful");
            System.out.println("File size: " + fileSize + " bytes");
            
            return true;
            
        } catch (IOException e) {
            System.err.println("Verification failed with exception: " + e.getMessage());
            return false;
        }
    }
    
    /**
     * List all deployed SMED maps in current DISK/Library
     */
    public List<String> listDeployedMaps() {
        List<String> deployedMaps = new ArrayList<>();
        
        try {
            Path deployPath = Paths.get(deploymentPath);
            
            if (Files.exists(deployPath) && Files.isDirectory(deployPath)) {
                Files.walk(deployPath, 1)
                     .filter(path -> path.toString().endsWith(".json"))
                     .forEach(path -> deployedMaps.add(path.getFileName().toString()));
            }
            
        } catch (IOException e) {
            System.err.println("Failed to list deployed maps: " + e.getMessage());
        }
        
        return deployedMaps;
    }
    
    /**
     * Get deployment statistics
     */
    public DeploymentStats getDeploymentStats() {
        DeploymentStats stats = new DeploymentStats();
        stats.setTargetDisk(targetDisk);
        stats.setTargetLibrary(targetLibrary);
        stats.setDeploymentPath(deploymentPath);
        
        try {
            Path deployPath = Paths.get(deploymentPath);
            
            if (Files.exists(deployPath) && Files.isDirectory(deployPath)) {
                List<Path> jsonFiles = new ArrayList<>();
                Files.walk(deployPath, 1)
                     .filter(path -> path.toString().endsWith(".json"))
                     .forEach(jsonFiles::add);
                
                stats.setTotalMaps(jsonFiles.size());
                
                long totalSize = 0;
                for (Path file : jsonFiles) {
                    totalSize += Files.size(file);
                }
                stats.setTotalSize(totalSize);
                
                if (!jsonFiles.isEmpty()) {
                    // Get latest deployment time
                    Optional<Path> latestFile = jsonFiles.stream()
                        .max(Comparator.comparing(path -> {
                            try {
                                return Files.getLastModifiedTime(path);
                            } catch (IOException e) {
                                return FileTime.fromMillis(0);
                            }
                        }));
                    
                    if (latestFile.isPresent()) {
                        stats.setLastDeploymentTime(
                            Files.getLastModifiedTime(latestFile.get()).toMillis()
                        );
                    }
                }
            }
            
        } catch (IOException e) {
            System.err.println("Failed to get deployment stats: " + e.getMessage());
        }
        
        return stats;
    }
    
    // Setters for configuration
    public void setTargetDisk(String disk) {
        this.targetDisk = disk != null ? disk : DEFAULT_DISK;
        updateDeploymentPath();
    }
    
    public void setTargetLibrary(String library) {
        this.targetLibrary = library != null ? library : DEFAULT_LIBRARY;
        updateDeploymentPath();
    }
    
    // Getters
    public String getTargetDisk() { return targetDisk; }
    public String getTargetLibrary() { return targetLibrary; }
    public String getDeploymentPath() { return deploymentPath; }
    
    // Data classes
    public static class DeploymentResult {
        private boolean success;
        private String errorMessage;
        private String sourceFile;
        private String targetFilePath;
        private String outputFileName;
        private String targetDisk;
        private String targetLibrary;
        private Date deploymentTime;
        private int deployedFields;
        
        // Getters and setters
        public boolean isSuccess() { return success; }
        public void setSuccess(boolean success) { this.success = success; }
        
        public String getErrorMessage() { return errorMessage; }
        public void setErrorMessage(String errorMessage) { this.errorMessage = errorMessage; }
        
        public String getSourceFile() { return sourceFile; }
        public void setSourceFile(String sourceFile) { this.sourceFile = sourceFile; }
        
        public String getTargetFilePath() { return targetFilePath; }
        public void setTargetFilePath(String targetFilePath) { this.targetFilePath = targetFilePath; }
        
        public String getOutputFileName() { return outputFileName; }
        public void setOutputFileName(String outputFileName) { this.outputFileName = outputFileName; }
        
        public String getTargetDisk() { return targetDisk; }
        public void setTargetDisk(String targetDisk) { this.targetDisk = targetDisk; }
        
        public String getTargetLibrary() { return targetLibrary; }
        public void setTargetLibrary(String targetLibrary) { this.targetLibrary = targetLibrary; }
        
        public Date getDeploymentTime() { return deploymentTime; }
        public void setDeploymentTime(Date deploymentTime) { this.deploymentTime = deploymentTime; }
        
        public int getDeployedFields() { return deployedFields; }
        public void setDeployedFields(int deployedFields) { this.deployedFields = deployedFields; }
    }
    
    public static class DeploymentStats {
        private String targetDisk;
        private String targetLibrary;
        private String deploymentPath;
        private int totalMaps;
        private long totalSize;
        private long lastDeploymentTime;
        
        // Getters and setters
        public String getTargetDisk() { return targetDisk; }
        public void setTargetDisk(String targetDisk) { this.targetDisk = targetDisk; }
        
        public String getTargetLibrary() { return targetLibrary; }
        public void setTargetLibrary(String targetLibrary) { this.targetLibrary = targetLibrary; }
        
        public String getDeploymentPath() { return deploymentPath; }
        public void setDeploymentPath(String deploymentPath) { this.deploymentPath = deploymentPath; }
        
        public int getTotalMaps() { return totalMaps; }
        public void setTotalMaps(int totalMaps) { this.totalMaps = totalMaps; }
        
        public long getTotalSize() { return totalSize; }
        public void setTotalSize(long totalSize) { this.totalSize = totalSize; }
        
        public long getLastDeploymentTime() { return lastDeploymentTime; }
        public void setLastDeploymentTime(long lastDeploymentTime) { this.lastDeploymentTime = lastDeploymentTime; }
    }
}