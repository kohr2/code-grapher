#!/usr/bin/env python3
"""
TypeScript Relationship Extractor Service
Python wrapper for the Node.js TypeScript relationship extractor
Integrates with the existing service architecture
"""

import json
import subprocess
import os
from pathlib import Path
from typing import Dict, List, Any, Optional
import logging

from core.interfaces.ts_relationship_extractor_interface import (
    TSRelationshipExtractorInterface, 
    JSRelationshipExtraction, 
    JSEntityInfo, 
    JSRelationshipType
)
from js_entity_classifier import classify_js_entities, JSEntityClassifier
# from logger import logger  # Using print for now due to logger interface issues


class TSRelationshipExtractorService(TSRelationshipExtractorInterface):
    """
    TypeScript relationship extractor service implementation
    Bridges Python service architecture with Node.js TypeScript analysis
    """
    
    def __init__(self, project_root: Optional[str] = None):
        self.project_root = project_root or os.getcwd()
        self.node_script_path = os.path.join(os.path.dirname(__file__), "ts_relationship_extractor.js")
        # self.logger = logger  # Using print for now
        self.entity_classifier = JSEntityClassifier()
        
        # Verify Node.js script exists
        if not os.path.exists(self.node_script_path):
            raise FileNotFoundError(f"TypeScript extractor script not found: {self.node_script_path}")
    
    def extract_relationships(self, file_paths: List[str], 
                            project_root: Optional[str] = None) -> List[JSRelationshipExtraction]:
        """
        Extract all relationships from TypeScript/JavaScript files
        """
        root = project_root or self.project_root
        
        try:
            # Prepare command
            cmd = [
                "node", 
                self.node_script_path, 
                "extract-relationships"
            ] + file_paths + ["--project-root", root]
            
            print(f"🔍 Extracting JS/TS relationships from {len(file_paths)} files")
            
            # Execute Node.js script
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                cwd=os.path.dirname(self.node_script_path),
                timeout=300  # 5 minute timeout
            )
            
            if result.returncode != 0:
                print(f"❌ TypeScript extractor failed: {result.stderr}")
                return []
            
            # Parse JSON output
            relationships_data = json.loads(result.stdout)
            
            # Convert to JSRelationshipExtraction objects
            relationships = []
            for rel_data in relationships_data:
                try:
                    relationship = JSRelationshipExtraction(
                        source_file=rel_data["source_file"],
                        target_file=rel_data["target_file"],
                        source_entity=rel_data["source_entity"],
                        target_entity=rel_data["target_entity"],
                        relationship_type=JSRelationshipType(rel_data["relationship_type"]),
                        confidence=rel_data["confidence"],
                        relationship_strength=rel_data["relationship_strength"],
                        line_number=rel_data.get("line_number"),
                        context=rel_data.get("context"),
                        metadata=rel_data.get("metadata", {})
                    )
                    relationships.append(relationship)
                except (KeyError, ValueError, TypeError) as e:
                    print(f"⚠️ Failed to parse relationship: {e}")
                    continue
            
            print(f"✅ Extracted {len(relationships)} JS/TS relationships")
            return relationships
            
        except subprocess.TimeoutExpired:
            print("❌ TypeScript extractor timed out")
            return []
        except json.JSONDecodeError as e:
            print(f"❌ Failed to parse TypeScript extractor output: {e}")
            return []
        except Exception as e:
            print(f"❌ TypeScript extractor error: {e}")
            return []
    
    def extract_entities(self, file_paths: List[str],
                        project_root: Optional[str] = None) -> List[JSEntityInfo]:
        """
        Extract all entities from TypeScript/JavaScript files
        """
        root = project_root or self.project_root
        
        try:
            # Prepare command
            cmd = [
                "node",
                self.node_script_path,
                "extract-entities"
            ] + file_paths + ["--project-root", root]
            
            print(f"🔍 Extracting JS/TS entities from {len(file_paths)} files")
            
            # Execute Node.js script
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                cwd=os.path.dirname(self.node_script_path),
                timeout=300
            )
            
            if result.returncode != 0:
                print(f"TypeScript entity extractor failed: {result.stderr}")
                return []
            
            # Parse JSON output
            entities_data = json.loads(result.stdout)
            
            # Convert to JSEntityInfo objects
            entities = []
            for entity_data in entities_data:
                try:
                    entity = JSEntityInfo(
                        name=entity_data["name"],
                        type=entity_data["type"],
                        file_path=entity_data["file_path"],
                        line_number=entity_data["line_number"],
                        specialized_type=entity_data.get("specialized_type"),
                        category=entity_data.get("category"),
                        metadata=entity_data.get("metadata", {}),
                        parameters=entity_data.get("parameters"),
                        return_type=entity_data.get("return_type"),
                        decorators=entity_data.get("decorators"),
                        base_classes=entity_data.get("base_classes"),
                        interfaces=entity_data.get("interfaces"),
                        type_parameters=entity_data.get("type_parameters"),
                        is_async=entity_data.get("is_async", False),
                        is_exported=entity_data.get("is_exported", False),
                        is_default_export=entity_data.get("is_default_export", False)
                    )
                    entities.append(entity)
                except (KeyError, TypeError) as e:
                    print(f"Failed to parse entity: {e}")
                    continue
            
            print(f"✅ Extracted {len(entities)} JS/TS entities")
            return entities
            
        except subprocess.TimeoutExpired:
            print("TypeScript entity extractor timed out")
            return []
        except json.JSONDecodeError as e:
            print(f"Failed to parse TypeScript entity extractor output: {e}")
            return []
        except Exception as e:
            print(f"TypeScript entity extractor error: {e}")
            return []
    
    def analyze_file(self, file_path: str,
                    project_root: Optional[str] = None) -> Dict[str, Any]:
        """
        Analyze a single TypeScript/JavaScript file
        """
        root = project_root or self.project_root
        
        try:
            # Prepare command
            cmd = [
                "node",
                self.node_script_path,
                "analyze",
                file_path,
                root
            ]
            
            print(f"🔍 Analyzing JS/TS file: {file_path}")
            
            # Execute Node.js script
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                cwd=os.path.dirname(self.node_script_path),
                timeout=60
            )
            
            if result.returncode != 0:
                print(f"TypeScript file analyzer failed: {result.stderr}")
                return {
                    "entities": [],
                    "relationships": [],
                    "file_path": file_path,
                    "success": False,
                    "error": result.stderr
                }
            
            # Parse JSON output
            analysis_result = json.loads(result.stdout)
            
            # Enhance entities with classification
            if analysis_result.get("success", False) and analysis_result.get("entities"):
                file_context = {"file_path": file_path, "project_root": root}
                enhanced_entities = classify_js_entities(analysis_result["entities"], file_context)
                analysis_result["entities"] = enhanced_entities
            
            return analysis_result
            
        except subprocess.TimeoutExpired:
            print(f"TypeScript file analyzer timed out for: {file_path}")
            return {
                "entities": [],
                "relationships": [],
                "file_path": file_path,
                "success": False,
                "error": "Timeout"
            }
        except json.JSONDecodeError as e:
            print(f"Failed to parse file analysis output: {e}")
            return {
                "entities": [],
                "relationships": [],
                "file_path": file_path,
                "success": False,
                "error": f"JSON decode error: {e}"
            }
        except Exception as e:
            print(f"File analysis error: {e}")
            return {
                "entities": [],
                "relationships": [],
                "file_path": file_path,
                "success": False,
                "error": str(e)
            }
    
    def get_supported_extensions(self) -> List[str]:
        """
        Get list of supported file extensions
        """
        return ['.ts', '.tsx', '.js', '.jsx']
    
    def validate_project_structure(self, project_root: str) -> bool:
        """
        Validate that the project structure is compatible with the extractor
        """
        try:
            # Check for package.json or tsconfig.json
            package_json = os.path.join(project_root, 'package.json')
            tsconfig_json = os.path.join(project_root, 'tsconfig.json')
            
            has_package_json = os.path.exists(package_json)
            has_tsconfig = os.path.exists(tsconfig_json)
            
            print(f"Project validation - package.json: {has_package_json}, tsconfig.json: {has_tsconfig}")
            
            return has_package_json or has_tsconfig
            
        except Exception as e:
            print(f"Project validation error: {e}")
            return False
    
    def find_js_ts_files(self, root_path: str, exclude_patterns: Optional[List[str]] = None) -> List[str]:
        """
        Find all JavaScript/TypeScript files in a directory tree
        """
        exclude_patterns = exclude_patterns or ['node_modules', '.git', 'dist', 'build', '.next']
        supported_extensions = self.get_supported_extensions()
        
        js_ts_files = []
        root = Path(root_path)
        
        try:
            for file_path in root.rglob('*'):
                if file_path.is_file() and file_path.suffix in supported_extensions:
                    # Check if file should be excluded
                    path_str = str(file_path)
                    if not any(pattern in path_str for pattern in exclude_patterns):
                        js_ts_files.append(str(file_path))
                        
            print(f"Found {len(js_ts_files)} JS/TS files in {root_path}")
            return js_ts_files
            
        except Exception as e:
            print(f"Error finding JS/TS files: {e}")
            return []
    
    def extract_relationships_from_project(self, project_root: str, 
                                         exclude_patterns: Optional[List[str]] = None) -> List[JSRelationshipExtraction]:
        """
        Extract relationships from all JavaScript/TypeScript files in a project
        """
        if not self.validate_project_structure(project_root):
            print(f"Project structure validation failed for: {project_root}")
        
        # Find all JS/TS files
        js_ts_files = self.find_js_ts_files(project_root, exclude_patterns)
        
        if not js_ts_files:
            print(f"No JS/TS files found in: {project_root}")
            return []
        
        # Extract relationships
        return self.extract_relationships(js_ts_files, project_root)
    
    def extract_entities_from_project(self, project_root: str,
                                    exclude_patterns: Optional[List[str]] = None) -> List[JSEntityInfo]:
        """
        Extract entities from all JavaScript/TypeScript files in a project
        """
        if not self.validate_project_structure(project_root):
            print(f"Project structure validation failed for: {project_root}")
        
        # Find all JS/TS files
        js_ts_files = self.find_js_ts_files(project_root, exclude_patterns)
        
        if not js_ts_files:
            print(f"No JS/TS files found in: {project_root}")
            return []
        
        # Extract entities
        return self.extract_entities(js_ts_files, project_root)


# Factory function for service creation
def create_ts_relationship_extractor(project_root: Optional[str] = None) -> TSRelationshipExtractorService:
    """Create a TypeScript relationship extractor service instance"""
    return TSRelationshipExtractorService(project_root)


# CLI interface for testing
def main():
    import sys
    
    if len(sys.argv) < 3:
        print("Usage: python ts_relationship_extractor_service.py <command> <path>")
        print("Commands:")
        print("  analyze-file <file_path>")
        print("  analyze-project <project_root>")
        print("  find-files <project_root>")
        sys.exit(1)
    
    command = sys.argv[1]
    path = sys.argv[2]
    
    extractor = TSRelationshipExtractorService()
    
    try:
        if command == "analyze-file":
            result = extractor.analyze_file(path)
            print(json.dumps(result, indent=2))
            
        elif command == "analyze-project":
            entities = extractor.extract_entities_from_project(path)
            relationships = extractor.extract_relationships_from_project(path)
            
            result = {
                "entities": [entity.__dict__ for entity in entities],
                "relationships": [rel.__dict__ for rel in relationships],
                "project_root": path,
                "success": True
            }
            print(json.dumps(result, indent=2, default=str))
            
        elif command == "find-files":
            files = extractor.find_js_ts_files(path)
            print(json.dumps(files, indent=2))
            
        else:
            print(f"Unknown command: {command}")
            sys.exit(1)
            
    except Exception as e:
        print(f"Error: {e}")
        sys.exit(1)


if __name__ == "__main__":
    main()