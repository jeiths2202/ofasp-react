package com.openasp.smed;

import java.util.*;

/**
 * SMED System Controller - Main integration class for 1차 목표 사양
 * 
 * Implementation Priority:
 * 1. SmedMapParser.java - Legacy SMED 파싱 및 JSON 변환
 * 2. SmedMapDeployer.java - JSON 파일 배포
 * 3. SmedMapViewer.java - JSON 읽어서 웹 그리드 생성
 * 
 * Features:
 * - SJIS-encoded Legacy SMED map file parsing
 * - JSON conversion with field type classification (PROMPT 기반)
 * - Deployment to /home/aspuser/app/volume/{DISK}/{LIBRARY}/ as SMED맵명.json
 * - 24행 x 80열 웹 그리드 디스플레이
 */
public class SMEDSystemController {
    
    private SMEDMapParser parser;
    private SMEDDeploymentSystem deployer;
    private SMEDWebGridDisplay webDisplay;
    
    public SMEDSystemController() {
        this.parser = new SMEDMapParser();
        this.deployer = new SMEDDeploymentSystem();
        this.webDisplay = new SMEDWebGridDisplay();
    }
    
    public SMEDSystemController(String disk, String library) {
        this.parser = new SMEDMapParser();
        this.deployer = new SMEDDeploymentSystem(disk, library);
        this.webDisplay = new SMEDWebGridDisplay();
    }
    
    /**
     * Complete SMED processing workflow
     * Parse → Deploy → Generate Web Display
     */
    public WorkflowResult processCompleteWorkflow(String smedFilePath, Map<String, String> displayData) {
        WorkflowResult result = new WorkflowResult();
        result.setSourceFile(smedFilePath);
        result.setStartTime(new Date());
        
        try {
            System.out.println("=== SMED Complete Workflow Started ===");
            System.out.println("Source File: " + smedFilePath);
            System.out.println("Target DISK: " + deployer.getTargetDisk());
            System.out.println("Target Library: " + deployer.getTargetLibrary());
            
            // Step 1: Parse SMED file
            System.out.println("\n--- Step 1: Parsing SMED File ---");
            SMEDMapParser.ParseResult parseResult = parser.parseSMEDFile(smedFilePath);
            if (!parseResult.isSuccess()) {
                result.setSuccess(false);
                result.setErrorMessage("Parse failed: " + parseResult.getErrorMessage());
                return result;
            }
            result.setParseResult(parseResult);
            
            // Step 2: Deploy to target location
            System.out.println("\n--- Step 2: Deploying SMED Map ---");
            SMEDDeploymentSystem.DeploymentResult deployResult = deployer.deploySMEDMap(smedFilePath);
            if (!deployResult.isSuccess()) {
                result.setSuccess(false);
                result.setErrorMessage("Deployment failed: " + deployResult.getErrorMessage());
                return result;
            }
            result.setDeploymentResult(deployResult);
            
            // Step 3: Generate web display
            System.out.println("\n--- Step 3: Generating Web Display ---");
            SMEDWebGridDisplay.DisplayResult displayResult = 
                webDisplay.displaySMEDMap(deployResult.getTargetFilePath(), displayData);
            if (!displayResult.isSuccess()) {
                result.setSuccess(false);
                result.setErrorMessage("Web display failed: " + displayResult.getErrorMessage());
                return result;
            }
            result.setDisplayResult(displayResult);
            
            // Step 4: Save HTML output
            String htmlOutputPath = deployResult.getTargetFilePath().replace(".json", ".html");
            if (webDisplay.saveHTMLOutput(displayResult.getHtmlOutput(), htmlOutputPath)) {
                result.setHtmlOutputPath(htmlOutputPath);
            }
            
            result.setSuccess(true);
            result.setEndTime(new Date());
            
            System.out.println("\n=== SMED Complete Workflow Completed ===");
            System.out.println("JSON Output: " + deployResult.getTargetFilePath());
            System.out.println("HTML Output: " + htmlOutputPath);
            System.out.println("Processing Time: " + 
                (result.getEndTime().getTime() - result.getStartTime().getTime()) + "ms");
            
        } catch (Exception e) {
            result.setSuccess(false);
            result.setErrorMessage("Workflow exception: " + e.getMessage());
            e.printStackTrace();
        }
        
        return result;
    }
    
    /**
     * Parse SMED file only
     */
    public SMEDMapParser.ParseResult parseOnly(String smedFilePath) {
        return parser.parseSMEDFile(smedFilePath);
    }
    
    /**
     * Deploy SMED file only
     */
    public SMEDDeploymentSystem.DeploymentResult deployOnly(String smedFilePath) {
        return deployer.deploySMEDMap(smedFilePath);
    }
    
    /**
     * Generate web display from existing JSON file
     */
    public SMEDWebGridDisplay.DisplayResult displayOnly(String jsonFilePath, Map<String, String> data) {
        return webDisplay.displaySMEDMap(jsonFilePath, data);
    }
    
    /**
     * Get system status and statistics
     */
    public SystemStatus getSystemStatus() {
        SystemStatus status = new SystemStatus();
        
        // Get deployment statistics
        SMEDDeploymentSystem.DeploymentStats deployStats = deployer.getDeploymentStats();
        status.setDeploymentStats(deployStats);
        
        // Get deployed maps list
        List<String> deployedMaps = deployer.listDeployedMaps();
        status.setDeployedMaps(deployedMaps);
        
        // System configuration
        status.setTargetDisk(deployer.getTargetDisk());
        status.setTargetLibrary(deployer.getTargetLibrary());
        status.setDeploymentPath(deployer.getDeploymentPath());
        
        return status;
    }
    
    /**
     * Configure deployment target
     */
    public void configureDeployment(String disk, String library) {
        deployer.setTargetDisk(disk);
        deployer.setTargetLibrary(library);
    }
    
    // Data classes
    public static class WorkflowResult {
        private boolean success;
        private String errorMessage;
        private String sourceFile;
        private Date startTime;
        private Date endTime;
        private SMEDMapParser.ParseResult parseResult;
        private SMEDDeploymentSystem.DeploymentResult deploymentResult;
        private SMEDWebGridDisplay.DisplayResult displayResult;
        private String htmlOutputPath;
        
        // Getters and setters
        public boolean isSuccess() { return success; }
        public void setSuccess(boolean success) { this.success = success; }
        
        public String getErrorMessage() { return errorMessage; }
        public void setErrorMessage(String errorMessage) { this.errorMessage = errorMessage; }
        
        public String getSourceFile() { return sourceFile; }
        public void setSourceFile(String sourceFile) { this.sourceFile = sourceFile; }
        
        public Date getStartTime() { return startTime; }
        public void setStartTime(Date startTime) { this.startTime = startTime; }
        
        public Date getEndTime() { return endTime; }
        public void setEndTime(Date endTime) { this.endTime = endTime; }
        
        public SMEDMapParser.ParseResult getParseResult() { return parseResult; }
        public void setParseResult(SMEDMapParser.ParseResult parseResult) { this.parseResult = parseResult; }
        
        public SMEDDeploymentSystem.DeploymentResult getDeploymentResult() { return deploymentResult; }
        public void setDeploymentResult(SMEDDeploymentSystem.DeploymentResult deploymentResult) { 
            this.deploymentResult = deploymentResult; 
        }
        
        public SMEDWebGridDisplay.DisplayResult getDisplayResult() { return displayResult; }
        public void setDisplayResult(SMEDWebGridDisplay.DisplayResult displayResult) { 
            this.displayResult = displayResult; 
        }
        
        public String getHtmlOutputPath() { return htmlOutputPath; }
        public void setHtmlOutputPath(String htmlOutputPath) { this.htmlOutputPath = htmlOutputPath; }
    }
    
    public static class SystemStatus {
        private String targetDisk;
        private String targetLibrary;
        private String deploymentPath;
        private SMEDDeploymentSystem.DeploymentStats deploymentStats;
        private List<String> deployedMaps;
        
        // Getters and setters
        public String getTargetDisk() { return targetDisk; }
        public void setTargetDisk(String targetDisk) { this.targetDisk = targetDisk; }
        
        public String getTargetLibrary() { return targetLibrary; }
        public void setTargetLibrary(String targetLibrary) { this.targetLibrary = targetLibrary; }
        
        public String getDeploymentPath() { return deploymentPath; }
        public void setDeploymentPath(String deploymentPath) { this.deploymentPath = deploymentPath; }
        
        public SMEDDeploymentSystem.DeploymentStats getDeploymentStats() { return deploymentStats; }
        public void setDeploymentStats(SMEDDeploymentSystem.DeploymentStats deploymentStats) { 
            this.deploymentStats = deploymentStats; 
        }
        
        public List<String> getDeployedMaps() { return deployedMaps; }
        public void setDeployedMaps(List<String> deployedMaps) { this.deployedMaps = deployedMaps; }
    }
}