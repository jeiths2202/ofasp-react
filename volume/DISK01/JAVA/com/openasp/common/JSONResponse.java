package com.openasp.common;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ObjectNode;
import java.util.HashMap;
import java.util.Map;

/**
 * JSONResponse class for standardized JSON responses in OpenASP
 * This class provides a consistent response format for all programs
 */
public class JSONResponse {
    private boolean success;
    private String message;
    private String error;
    private Map<String, Object> data;
    private int returnCode;
    private long timestamp;
    
    public JSONResponse() {
        this.data = new HashMap<>();
        this.timestamp = System.currentTimeMillis();
        this.success = true;
        this.returnCode = 0;
    }
    
    public JSONResponse(boolean success) {
        this();
        this.success = success;
    }
    
    // Getters and setters
    public boolean isSuccess() { return success; }
    public void setSuccess(boolean success) { this.success = success; }
    
    public String getMessage() { return message; }
    public void setMessage(String message) { this.message = message; }
    
    public String getError() { return error; }
    public void setError(String error) { this.error = error; }
    
    public Map<String, Object> getData() { return data; }
    public void setData(Map<String, Object> data) { this.data = data; }
    
    public int getReturnCode() { return returnCode; }
    public void setReturnCode(int returnCode) { this.returnCode = returnCode; }
    
    public long getTimestamp() { return timestamp; }
    public void setTimestamp(long timestamp) { this.timestamp = timestamp; }
    
    // Convenience methods
    public void set(String key, Object value) {
        this.data.put(key, value);
    }
    
    public Object get(String key) {
        return this.data.get(key);
    }
    
    // Convert to JSON string
    @Override
    public String toString() {
        try {
            ObjectMapper mapper = new ObjectMapper();
            ObjectNode node = mapper.createObjectNode();
            node.put("success", success);
            if (message != null) node.put("message", message);
            if (error != null) node.put("error", error);
            node.put("returnCode", returnCode);
            node.put("timestamp", timestamp);
            
            // Add data fields
            ObjectNode dataNode = mapper.createObjectNode();
            for (Map.Entry<String, Object> entry : data.entrySet()) {
                if (entry.getValue() instanceof String) {
                    dataNode.put(entry.getKey(), (String) entry.getValue());
                } else if (entry.getValue() instanceof Integer) {
                    dataNode.put(entry.getKey(), (Integer) entry.getValue());
                } else if (entry.getValue() instanceof Boolean) {
                    dataNode.put(entry.getKey(), (Boolean) entry.getValue());
                } else if (entry.getValue() instanceof Long) {
                    dataNode.put(entry.getKey(), (Long) entry.getValue());
                } else {
                    dataNode.put(entry.getKey(), entry.getValue().toString());
                }
            }
            node.set("data", dataNode);
            
            return mapper.writerWithDefaultPrettyPrinter().writeValueAsString(node);
        } catch (Exception e) {
            return "{\"error\":\"Failed to convert to JSON\"}";
        }
    }
}