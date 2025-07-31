#!/usr/bin/env python3
"""
Simple script to populate the graph with basic code analysis
Bypasses the complex pipeline import issues
"""

import os
import ast
import sys
import glob
from pathlib import Path

# Add current directory to path
sys.path.append('.')

from graph_manager import CodeGraphManager

def analyze_python_file(file_path):
    """Simple AST analysis of a Python file"""
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            content = f.read()
        
        # Parse AST
        tree = ast.parse(content)
        
        # Extract basic info
        classes = []
        functions = []
        imports = []
        
        for node in ast.walk(tree):
            if isinstance(node, ast.ClassDef):
                classes.append(node.name)
            elif isinstance(node, ast.FunctionDef):
                functions.append(node.name)
            elif isinstance(node, ast.Import):
                for alias in node.names:
                    imports.append(alias.name)
            elif isinstance(node, ast.ImportFrom):
                if node.module:
                    imports.append(node.module)
        
        return {
            "language": "python",
            "size": len(content),
            "last_modified": str(os.path.getmtime(file_path)),
            "classes": classes,
            "functions": functions,
            "imports": imports
        }
    except Exception as e:
        print(f"Error analyzing {file_path}: {e}")
        return None

def populate_graph():
    """Populate graph with simple analysis"""
    print("🚀 Starting simple graph population...")
    
    # Initialize graph manager
    gm = CodeGraphManager()
    
    # Find Python files in current directory
    python_files = []
    for pattern in ['*.py', '**/*.py']:
        python_files.extend(glob.glob(pattern, recursive=True))
    
    # Filter out hidden directories and large directories
    python_files = [f for f in python_files if not any(part.startswith('.') for part in Path(f).parts)]
    python_files = [f for f in python_files if 'venv' not in f and '__pycache__' not in f]
    
    print(f"📁 Found {len(python_files)} Python files")
    
    processed = 0
    entities_created = 0
    
    for file_path in python_files[:20]:  # Limit to first 20 files for testing
        print(f"📄 Processing: {file_path}")
        
        # Analyze file
        analysis = analyze_python_file(file_path)
        if not analysis:
            continue
        
        # Create file entity
        file_entity = gm.create_code_entity(
            "File",
            file_path,
            {
                "path": file_path,
                "language": analysis["language"],
                "size": analysis["size"],
                "last_modified": analysis["last_modified"]
            }
        )
        entities_created += 1
        
        # Create class entities
        for class_name in analysis["classes"]:
            class_entity = gm.create_code_entity(
                "Class",
                class_name,
                {
                    "name": class_name,
                    "file_path": file_path,
                    "type": "class"
                }
            )
            entities_created += 1
            
            # Create relationship
            if file_entity and class_entity:
                gm.create_relationship(file_entity, class_entity, "CONTAINS")
        
        # Create function entities
        for func_name in analysis["functions"]:
            func_entity = gm.create_code_entity(
                "Function",
                func_name,
                {
                    "name": func_name,
                    "file_path": file_path,
                    "type": "function"
                }
            )
            entities_created += 1
            
            # Create relationship
            if file_entity and func_entity:
                gm.create_relationship(file_entity, func_entity, "CONTAINS")
        
        processed += 1
    
    # Get final stats
    stats = gm.get_graph_stats()
    
    print(f"\n✅ Graph population complete!")
    print(f"📊 Files processed: {processed}")
    print(f"🎯 Entities created: {entities_created}")
    print(f"📈 Final graph stats: {stats}")
    
    gm.close()
    return stats

if __name__ == "__main__":
    populate_graph()