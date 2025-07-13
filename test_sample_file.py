"""
Sample file for testing surgical graph updates.
This file will be modified to test various change detection scenarios.
"""

import os
import json
import logging  # NEW IMPORT - should be detected
from typing import List, Dict, Any, Optional  # MODIFIED IMPORT - should be detected
from datetime import datetime, timedelta  # MODIFIED IMPORT - added timedelta


class TestDataProcessor:
    """A test class for processing data in various ways"""
    
    def __init__(self, config: Dict[str, Any], logger: Optional[logging.Logger] = None):  # MODIFIED FUNCTION - added parameter
        self.config = config
        self.processed_items = []
        self.start_time = datetime.now()
        self.logger = logger or logging.getLogger(__name__)  # NEW ATTRIBUTE
    
    def process_data(self, data: List[Dict]) -> List[Dict]:
        """Process a list of data items"""
        self.logger.info(f"Starting to process {len(data)} items")  # NEW LINE - added logging
        results = []
        
        for item in data:
            processed_item = self._process_single_item(item)
            results.append(processed_item)
            self.processed_items.append(processed_item)
        
        self.logger.info(f"Completed processing {len(results)} items")  # NEW LINE - added logging
        return results
    
    def _process_single_item(self, item: Dict) -> Dict:
        """Process a single data item"""
        processed = {
            "id": item.get("id", "unknown"),
            "timestamp": datetime.now().isoformat(),
            "original_data": item,
            "processed": True
        }
        
        # Add some business logic
        if item.get("priority") == "high":
            processed["urgent"] = True
        
        return processed
    
    def get_statistics(self) -> Dict[str, Any]:
        """Get processing statistics"""
        return {
            "total_processed": len(self.processed_items),
            "start_time": self.start_time.isoformat(),
            "processing_duration": (datetime.now() - self.start_time).total_seconds()
        }
    
    def reset_processor(self) -> None:  # NEW FUNCTION - should be detected as addition
        """Reset the processor state"""
        self.processed_items.clear()
        self.start_time = datetime.now()
        self.logger.info("Processor state has been reset")


def process_file_data(file_path: str) -> Dict[str, Any]:
    """Function to process data from a file"""
    if not os.path.exists(file_path):
        raise FileNotFoundError(f"File not found: {file_path}")
    
    with open(file_path, 'r') as f:
        data = json.load(f)
    
    processor = TestDataProcessor({"mode": "file"})
    results = processor.process_data(data)
    
    return {
        "file_path": file_path,
        "results": results,
        "statistics": processor.get_statistics()
    }


def calculate_metrics(data: List[Dict]) -> Dict[str, float]:
    """Calculate various metrics from processed data"""
    if not data:
        return {"count": 0, "average_size": 0.0, "total_size": 0}  # MODIFIED - added total_size to empty case
    
    total_size = sum(len(str(item)) for item in data)
    average_size = total_size / len(data)
    
    # NEW: Calculate additional metrics
    max_size = max(len(str(item)) for item in data)
    min_size = min(len(str(item)) for item in data)
    
    return {
        "count": len(data),
        "total_size": total_size,
        "average_size": average_size,
        "max_size": max_size,  # NEW FIELD
        "min_size": min_size   # NEW FIELD
    }


def validate_data_integrity(data: List[Dict]) -> Dict[str, Any]:  # NEW FUNCTION - should be detected
    """Validate the integrity of processed data"""
    issues = []
    
    for i, item in enumerate(data):
        if not isinstance(item, dict):
            issues.append(f"Item {i} is not a dictionary")
        elif "id" not in item:
            issues.append(f"Item {i} missing required 'id' field")
    
    return {
        "is_valid": len(issues) == 0,
        "total_items": len(data),
        "issues_found": issues,
        "validation_timestamp": datetime.now().isoformat()
    }


# Global configuration
DEFAULT_CONFIG = {
    "batch_size": 250,  # MODIFIED - increased from 100
    "timeout": 60,      # MODIFIED - increased from 30
    "retry_attempts": 5,  # MODIFIED - increased from 3
    "enable_logging": True  # NEW FIELD - should be detected
}

# NEW VARIABLE - should be detected
MAX_CONCURRENT_PROCESSES = 4

# Export main functions - MODIFIED to include new functions
__all__ = ["TestDataProcessor", "process_file_data", "calculate_metrics", "validate_data_integrity", "DEFAULT_CONFIG", "MAX_CONCURRENT_PROCESSES"]