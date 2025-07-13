"""
Sample file for testing surgical graph updates.
This file will be modified to test various change detection scenarios.
"""

import os
import json
from typing import List, Dict, Any
from datetime import datetime


class TestDataProcessor:
    """A test class for processing data in various ways"""
    
    def __init__(self, config: Dict[str, Any]):
        self.config = config
        self.processed_items = []
        self.start_time = datetime.now()
    
    def process_data(self, data: List[Dict]) -> List[Dict]:
        """Process a list of data items"""
        results = []
        
        for item in data:
            processed_item = self._process_single_item(item)
            results.append(processed_item)
            self.processed_items.append(processed_item)
        
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
        return {"count": 0, "average_size": 0.0}
    
    total_size = sum(len(str(item)) for item in data)
    average_size = total_size / len(data)
    
    return {
        "count": len(data),
        "total_size": total_size,
        "average_size": average_size
    }


# Global configuration
DEFAULT_CONFIG = {
    "batch_size": 100,
    "timeout": 30,
    "retry_attempts": 3
}

# Export main functions
__all__ = ["TestDataProcessor", "process_file_data", "calculate_metrics", "DEFAULT_CONFIG"]