#!/usr/bin/env python3
"""
Simple COBOL Parser
Basic COBOL parsing for the Code Grapher pipeline
"""

import re
from typing import Dict, List, Any
from pathlib import Path


class COBOLParser:
    """Simple COBOL parser for basic entity extraction"""
    
    def __init__(self):
        self.logger = None  # Will be set if needed
        
    def parse_file(self, file_path: str) -> Dict[str, Any]:
        """
        Parse a COBOL file and extract basic entities
        
        Args:
            file_path: Path to the COBOL file
            
        Returns:
            Dictionary with parsed COBOL data
        """
        try:
            with open(file_path, 'r', encoding='utf-8') as f:
                content = f.read()
            
            # Basic COBOL parsing
            compilation_units = self._parse_cobol_content(content)
            
            return {
                "file_path": file_path,
                "language": "cobol",
                "parse_success": True,
                "entities": self._extract_entities(compilation_units),
                "compilation_units": compilation_units,
                "lines_of_code": len(content.splitlines()),
                "error": None
            }
            
        except Exception as e:
            return {
                "file_path": file_path,
                "language": "cobol",
                "parse_success": False,
                "entities": [],
                "compilation_units": [],
                "lines_of_code": 0,
                "error": str(e)
            }
    
    def _parse_cobol_content(self, content: str) -> List[Dict[str, Any]]:
        """Parse COBOL content into compilation units"""
        lines = content.splitlines()
        compilation_units = []
        
        current_cu = {
            "program_id": "unknown",
            "divisions": {}
        }
        
        current_division = None
        current_section = None
        
        for line_num, line in enumerate(lines, 1):
            line = line.strip()
            
            # Skip empty lines and comments
            if not line or line.startswith('*'):
                continue
                
            # Program ID
            if 'PROGRAM-ID' in line.upper():
                match = re.search(r'PROGRAM-ID\.\s+(\w+)', line, re.IGNORECASE)
                if match:
                    current_cu["program_id"] = match.group(1)
            
            # Division headers
            elif any(div in line.upper() for div in ['IDENTIFICATION DIVISION', 'ENVIRONMENT DIVISION', 'DATA DIVISION', 'PROCEDURE DIVISION']):
                div_name = line.split()[0].lower()
                current_division = div_name
                current_cu["divisions"][div_name] = {}
                current_section = None
            
            # Section headers
            elif current_division and 'SECTION' in line.upper():
                section_name = line.split()[0].lower()
                current_section = section_name
                if current_division not in current_cu["divisions"]:
                    current_cu["divisions"][current_division] = {}
                current_cu["divisions"][current_division][section_name] = []
            
            # Paragraphs (in procedure division)
            elif current_division == "procedure" and line and not line.startswith(' ') and not line.startswith('\t'):
                # This is likely a paragraph name
                paragraph_name = line.split()[0]
                if "paragraphs" not in current_cu["divisions"]["procedure"]:
                    current_cu["divisions"]["procedure"]["paragraphs"] = []
                
                current_cu["divisions"]["procedure"]["paragraphs"].append({
                    "name": paragraph_name,
                    "line_number": line_num,
                    "statements": []
                })
            
            # Statements (in procedure division)
            elif current_division == "procedure" and line and (line.startswith(' ') or line.startswith('\t')):
                if "paragraphs" in current_cu["divisions"]["procedure"] and current_cu["divisions"]["procedure"]["paragraphs"]:
                    current_cu["divisions"]["procedure"]["paragraphs"][-1]["statements"].append(line.strip())
        
        compilation_units.append(current_cu)
        return compilation_units
    
    def _extract_entities(self, compilation_units: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """Extract entities from compilation units"""
        entities = []
        
        for cu in compilation_units:
            # Program entity
            entities.append({
                "type": "program",
                "name": cu.get("program_id", "unknown"),
                "line_number": 1,
                "language": "cobol"
            })
            
            # Paragraph entities (from procedure division)
            if "procedure" in cu.get("divisions", {}) and "paragraphs" in cu["divisions"]["procedure"]:
                for para in cu["divisions"]["procedure"]["paragraphs"]:
                    entities.append({
                        "type": "paragraph",
                        "name": para["name"],
                        "line_number": para["line_number"],
                        "language": "cobol"
                    })
        
        return entities


if __name__ == "__main__":
    print("🧪 COBOL Parser")
    print("   Use: COBOLParser().parse_file(file_path)")
