package com.openasp.cobol;

import java.io.IOException;
import java.util.logging.Logger;
import java.util.logging.Level;

/**
 * DslockServiceImpl - Implementation of DslockService
 * 
 * Provides JNI wrapper for dslock_suite C library
 * Handles dataset locking, reading, writing, and releasing
 */
public class DslockServiceImpl implements DslockService {
    
    private static final Logger logger = Logger.getLogger(DslockServiceImpl.class.getName());
    
    // Load native library
    static {
        try {
            System.loadLibrary("dslock_suite");
            logger.info("Successfully loaded dslock_suite native library");
        } catch (UnsatisfiedLinkError e) {
            logger.log(Level.SEVERE, "Failed to load dslock_suite library", e);
            throw new RuntimeException("Cannot load dslock_suite library: " + e.getMessage());
        }
    }
    
    // Native method declarations
    private native long nativeOpenDataset(String dataset, String mode, int level);
    private native byte[] nativeReadRecord(long handle);
    private native int nativeWriteRecord(long handle, byte[] data);
    private native int nativeCloseDataset(long handle);
    private native String nativeGetLockStatus(String dataset);
    private native String nativeGetDatasetInfo(long handle);
    
    @Override
    public DsioHandle openDataset(String dataset, String mode) throws Exception {
        logger.info("Opening dataset: " + dataset + " with mode: " + mode);
        
        // Determine lock level based on mode
        int lockLevel;
        String level;
        switch (mode.toUpperCase()) {
            case "READ":
                lockLevel = 0;  // SHR - Shared lock for reading
                level = "SHR";
                break;
            case "WRITE":
                lockLevel = 2;  // MOD - Modify lock for writing
                level = "MOD";
                break;
            case "READWRITE":
                lockLevel = 1;  // OLD - Exclusive lock for read/write
                level = "OLD";
                break;
            default:
                throw new IllegalArgumentException("Invalid mode: " + mode);
        }
        
        // Call native method to open dataset
        long nativeHandle = nativeOpenDataset(dataset, mode, lockLevel);
        
        if (nativeHandle == 0) {
            throw new IOException("Failed to open dataset: " + dataset);
        }
        
        // Create handle object
        DsioHandle handle = new DsioHandle(nativeHandle, dataset, level);
        
        // Get dataset attributes
        String info = nativeGetDatasetInfo(nativeHandle);
        if (info != null) {
            // Parse info format: "RECFM=FB,LRECL=80"
            String[] parts = info.split(",");
            for (String part : parts) {
                String[] kv = part.split("=");
                if (kv.length == 2) {
                    if ("RECFM".equals(kv[0])) {
                        handle.setRecfm(kv[1]);
                    } else if ("LRECL".equals(kv[0])) {
                        try {
                            handle.setLrecl(Integer.parseInt(kv[1]));
                        } catch (NumberFormatException e) {
                            logger.warning("Invalid LRECL value: " + kv[1]);
                        }
                    }
                }
            }
        }
        
        logger.info("Dataset opened successfully: " + handle);
        return handle;
    }
    
    @Override
    public byte[] readRecord(DsioHandle handle) throws Exception {
        if (handle == null || handle.isClosed()) {
            throw new IllegalStateException("Invalid or closed handle");
        }
        
        logger.fine("Reading record from dataset: " + handle.getDataset());
        
        // Call native method to read record
        byte[] data = nativeReadRecord(handle.getNativeHandle());
        
        if (data == null) {
            logger.fine("EOF reached for dataset: " + handle.getDataset());
            return null;  // EOF
        }
        
        logger.fine("Read " + data.length + " bytes from dataset");
        return data;
    }
    
    @Override
    public void writeRecord(DsioHandle handle, byte[] data) throws Exception {
        if (handle == null || handle.isClosed()) {
            throw new IllegalStateException("Invalid or closed handle");
        }
        
        if (data == null) {
            throw new IllegalArgumentException("Data cannot be null");
        }
        
        logger.fine("Writing " + data.length + " bytes to dataset: " + handle.getDataset());
        
        // Call native method to write record
        int result = nativeWriteRecord(handle.getNativeHandle(), data);
        
        if (result != 0) {
            throw new IOException("Failed to write record, error code: " + result);
        }
        
        logger.fine("Record written successfully");
    }
    
    @Override
    public void closeDataset(DsioHandle handle) throws Exception {
        if (handle == null || handle.isClosed()) {
            return;  // Already closed or invalid
        }
        
        logger.info("Closing dataset: " + handle.getDataset());
        
        try {
            // Call native method to close dataset and release lock
            int result = nativeCloseDataset(handle.getNativeHandle());
            
            if (result != 0) {
                logger.warning("Error closing dataset, code: " + result);
            }
            
            // Mark handle as closed
            handle.setClosed(true);
            
            logger.info("Dataset closed successfully");
            
        } catch (Exception e) {
            logger.log(Level.SEVERE, "Failed to close dataset", e);
            throw new IOException("Failed to close dataset: " + e.getMessage());
        }
    }
    
    @Override
    public String getLockStatus(String dataset) throws Exception {
        logger.fine("Getting lock status for dataset: " + dataset);
        
        // Call native method to get lock status
        String status = nativeGetLockStatus(dataset);
        
        if (status == null) {
            throw new IOException("Failed to get lock status for: " + dataset);
        }
        
        logger.fine("Lock status: " + status);
        return status;
    }
}