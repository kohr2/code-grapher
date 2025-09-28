#!/usr/bin/env python3
"""
COBOL Section Extractor
Extracts relevant sections and dependencies from COBOL programs for AI processing
Instead of sending the entire program, this sends only the relevant sections
"""

import re
import os
import sys
from typing import Dict, List, Any, Optional, Set, Tuple
from dataclasses import dataclass

# Add the project root to the path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)


@dataclass
class COBOLSection:
    """Represents a section of COBOL code"""
    name: str
    type: str  # 'paragraph', 'section', 'division', 'data_item'
    content: str
    line_start: int
    line_end: int
    dependencies: List[str]  # Names of other sections this depends on
    context: str  # Brief description of what this section does
    
    @property
    def line_count(self) -> int:
        """Calculate the number of lines in this section"""
        return self.line_end - self.line_start + 1
    
    @property
    def line_range(self) -> str:
        """Get the line range as a string"""
        return f"{self.line_start}-{self.line_end}"


class COBOLSectionExtractor:
    """Extracts relevant sections from COBOL programs for AI processing"""
    
    def __init__(self):
        self.section_patterns = {
            'paragraph': r'^([A-Z0-9-]+)\s*\.',
            'section': r'^([A-Z0-9-]+)\s+SECTION\s*\.',
            'data_item': r'^\s*(\d+)\s+([A-Z0-9-]+)',
            'file_description': r'^FD\s+([A-Z0-9-]+)',
            'working_storage': r'^WORKING-STORAGE\s+SECTION',
            'linkage_section': r'^LINKAGE\s+SECTION',
            'procedure_division': r'^PROCEDURE\s+DIVISION',
        }
    
    def extract_relevant_sections(self, cobol_content: str, target_entities: List[str] = None) -> Dict[str, COBOLSection]:
        """
        Extract relevant sections from COBOL content for AI processing
        
        Args:
            cobol_content: Full COBOL program content
            target_entities: List of entity names to focus on (for cross-file relationships)
            
        Returns:
            Dictionary of section name -> COBOLSection
        """
        lines = cobol_content.split('\n')
        sections = {}
        
        # First pass: identify all sections
        all_sections = self._identify_all_sections(lines)
        
        # Second pass: extract content for relevant sections
        if target_entities:
            # Focus on sections that interact with target entities
            relevant_sections = self._find_relevant_sections(all_sections, target_entities, lines)
        else:
            # Extract key sections: main program, data definitions, and procedure division
            relevant_sections = self._find_key_sections(all_sections, lines)
        
        # Third pass: extract content and dependencies
        for section_name in relevant_sections:
            section_info = all_sections[section_name]
            content = self._extract_section_content(section_info, lines)
            dependencies = self._find_section_dependencies(content, all_sections)
            
            sections[section_name] = COBOLSection(
                name=section_name,
                type=section_info['type'],
                content=content,
                line_start=section_info['line_start'],
                line_end=section_info['line_end'],
                dependencies=dependencies,
                context=self._generate_section_context(section_info, content)
            )
        
        return sections
    
    def create_optimized_prompt_content(self, sections: Dict[str, COBOLSection], 
                                      max_lines: int = 200) -> str:
        """
        Create optimized content for AI prompts by combining relevant sections
        
        Args:
            sections: Dictionary of COBOL sections
            max_lines: Maximum number of lines to include
            
        Returns:
            Optimized COBOL content for AI processing
        """
        if not sections:
            return ""
        
        # Sort sections by importance and dependencies
        sorted_sections = self._sort_sections_by_importance(sections)
        
        # Build content within line limit
        content_parts = []
        current_lines = 0
        
        # Add program identification first
        program_section = self._find_program_section(sections)
        if program_section:
            content_parts.append(f"// PROGRAM: {program_section.name}")
            content_parts.append(program_section.content[:500])  # Limit program section
            current_lines += len(program_section.content.split('\n'))
        
        # Add key data definitions
        data_sections = [s for s in sorted_sections if s.type in ['data_item', 'file_description']]
        for section in data_sections[:3]:  # Limit to 3 key data sections
            if current_lines >= max_lines:
                break
            content_parts.append(f"\n// DATA: {section.name}")
            section_lines = section.content.split('\n')
            content_parts.append('\n'.join(section_lines[:20]))  # Limit data section
            current_lines += min(20, len(section_lines))
        
        # Add procedure sections
        procedure_sections = [s for s in sorted_sections if s.type in ['paragraph', 'section']]
        for section in procedure_sections[:5]:  # Limit to 5 procedure sections
            if current_lines >= max_lines:
                break
            content_parts.append(f"\n// PROCEDURE: {section.name}")
            section_lines = section.content.split('\n')
            content_parts.append('\n'.join(section_lines[:30]))  # Limit procedure section
            current_lines += min(30, len(section_lines))
        
        # Add dependency summary
        all_dependencies = set()
        for section in sections.values():
            all_dependencies.update(section.dependencies)
        
        if all_dependencies:
            content_parts.append(f"\n// DEPENDENCIES: {', '.join(sorted(all_dependencies))}")
        
        return '\n'.join(content_parts)
    
    def _identify_all_sections(self, lines: List[str]) -> Dict[str, Dict[str, Any]]:
        """Identify all sections in the COBOL program"""
        sections = {}
        
        for i, line in enumerate(lines):
            # Remove line numbers (first 6 characters) and clean the line
            clean_line = line[6:].strip() if len(line) > 6 else line.strip()
            line_upper = clean_line.upper()
            
            # Check for paragraphs
            para_match = re.match(self.section_patterns['paragraph'], line_upper)
            if para_match:
                name = para_match.group(1)
                sections[name] = {
                    'type': 'paragraph',
                    'line_start': i,
                    'line_end': i,
                    'raw_line': line
                }
                continue
            
            # Check for sections
            section_match = re.match(self.section_patterns['section'], line_upper)
            if section_match:
                name = section_match.group(1)
                sections[name] = {
                    'type': 'section',
                    'line_start': i,
                    'line_end': i,
                    'raw_line': line
                }
                continue
            
            # Check for data items
            data_match = re.match(self.section_patterns['data_item'], line_upper)
            if data_match:
                level = data_match.group(1)
                name = data_match.group(2)
                if level in ['01', '77', '88']:  # Only top-level data items
                    sections[name] = {
                        'type': 'data_item',
                        'line_start': i,
                        'line_end': i,
                        'raw_line': line,
                        'level': level
                    }
                continue
            
            # Check for file descriptions
            fd_match = re.match(self.section_patterns['file_description'], line_upper)
            if fd_match:
                name = fd_match.group(1)
                sections[name] = {
                    'type': 'file_description',
                    'line_start': i,
                    'line_end': i,
                    'raw_line': line
                }
                continue
        
        # Update line_end for each section
        for name, section in sections.items():
            section['line_end'] = self._find_section_end(section['line_start'], lines)
        
        return sections
    
    def _find_section_end(self, start_line: int, lines: List[str]) -> int:
        """Find the end line of a section"""
        for i in range(start_line + 1, len(lines)):
            # Remove line numbers and clean the line
            clean_line = lines[i][6:].strip() if len(lines[i]) > 6 else lines[i].strip()
            if not clean_line or clean_line.startswith('*'):  # Empty line or comment
                continue
            
            # Check if this is the start of a new section
            if (re.match(self.section_patterns['paragraph'], clean_line.upper()) or
                re.match(self.section_patterns['section'], clean_line.upper()) or
                re.match(self.section_patterns['data_item'], clean_line.upper()) or
                re.match(self.section_patterns['file_description'], clean_line.upper())):
                return i - 1
            
            # Check for division boundaries
            if any(div in clean_line.upper() for div in ['IDENTIFICATION DIVISION', 'ENVIRONMENT DIVISION', 
                                                         'DATA DIVISION', 'PROCEDURE DIVISION']):
                return i - 1
        
        return len(lines) - 1
    
    def _find_relevant_sections(self, all_sections: Dict[str, Dict[str, Any]], 
                               target_entities: List[str], lines: List[str]) -> List[str]:
        """Find sections that are relevant to target entities"""
        relevant = set()
        
        for target in target_entities:
            # Direct matches
            if target in all_sections:
                relevant.add(target)
            
            # Find sections that reference the target
            for name, section in all_sections.items():
                content = self._extract_section_content(section, lines)
                if target.upper() in content.upper():
                    relevant.add(name)
        
        # If no relevant sections found, fall back to key sections
        if not relevant:
            relevant = set(self._find_key_sections(all_sections, lines))
        
        return list(relevant)
    
    def _find_key_sections(self, all_sections: Dict[str, Dict[str, Any]], 
                          lines: List[str]) -> List[str]:
        """Find key sections for general processing"""
        key_sections = set()
        
        # Always include main program
        for name, section in all_sections.items():
            if section['type'] in ['paragraph', 'section'] and 'MAIN' in name.upper():
                key_sections.add(name)
                break
        
        # Include important data items
        data_items = [name for name, section in all_sections.items() 
                     if section['type'] == 'data_item' and section.get('level') == '01']
        key_sections.update(data_items[:5])  # Top 5 data items
        
        # Include file descriptions
        file_descriptions = [name for name, section in all_sections.items() 
                           if section['type'] == 'file_description']
        key_sections.update(file_descriptions[:3])  # Top 3 file descriptions
        
        # Include procedure sections
        procedure_sections = [name for name, section in all_sections.items() 
                            if section['type'] in ['paragraph', 'section']]
        key_sections.update(procedure_sections[:8])  # Top 8 procedure sections
        
        return list(key_sections)
    
    def _extract_section_content(self, section: Dict[str, Any], lines: List[str]) -> str:
        """Extract the content of a section"""
        start = section['line_start']
        end = section['line_end']
        return '\n'.join(lines[start:end+1])
    
    def get_section_line_info(self, section: Dict[str, Any]) -> Dict[str, int]:
        """Get line information for a section"""
        start = section['line_start']
        end = section['line_end']
        return {
            'start_line': start,
            'end_line': end,
            'line_count': end - start + 1,
            'line_range': f"{start}-{end}"
        }
    
    def _find_section_dependencies(self, content: str, all_sections: Dict[str, Dict[str, Any]]) -> List[str]:
        """Find dependencies of a section"""
        dependencies = set()
        
        # Look for CALL statements
        call_matches = re.findall(r'CALL\s+["\']?([A-Z0-9-]+)["\']?', content.upper())
        dependencies.update(call_matches)
        
        # Look for PERFORM statements
        perform_matches = re.findall(r'PERFORM\s+([A-Z0-9-]+)', content.upper())
        dependencies.update(perform_matches)
        
        # Look for COPY statements
        copy_matches = re.findall(r'COPY\s+([A-Z0-9-]+)', content.upper())
        dependencies.update(copy_matches)
        
        # Look for variable references
        var_matches = re.findall(r'\b([A-Z0-9-]+)\b', content.upper())
        for var in var_matches:
            if var in all_sections and all_sections[var]['type'] == 'data_item':
                dependencies.add(var)
        
        return list(dependencies)
    
    def _generate_section_context(self, section: Dict[str, Any], content: str) -> str:
        """Generate a brief context description for a section"""
        section_type = section['type']
        name = section.get('name', 'unknown')
        
        if section_type == 'paragraph':
            return f"Paragraph {name} - contains procedure logic"
        elif section_type == 'section':
            return f"Section {name} - contains related procedures"
        elif section_type == 'data_item':
            level = section.get('level', '01')
            return f"Data item {name} (level {level}) - data definition"
        elif section_type == 'file_description':
            return f"File description {name} - file definition"
        else:
            return f"{section_type} {name}"
    
    def _sort_sections_by_importance(self, sections: Dict[str, COBOLSection]) -> List[COBOLSection]:
        """Sort sections by importance for AI processing"""
        def importance_key(section: COBOLSection) -> Tuple[int, int]:
            # Priority: main program > data items > procedure sections > others
            type_priority = {
                'paragraph': 1,
                'section': 2,
                'data_item': 3,
                'file_description': 4
            }
            
            # Prefer sections with more dependencies (more important)
            dependency_count = len(section.dependencies)
            
            return (type_priority.get(section.type, 5), -dependency_count)
        
        return sorted(sections.values(), key=importance_key)
    
    def _find_program_section(self, sections: Dict[str, COBOLSection]) -> Optional[COBOLSection]:
        """Find the main program section"""
        for section in sections.values():
            if section.type in ['paragraph', 'section'] and 'MAIN' in section.name.upper():
                return section
        
        # If no MAIN section, return the first procedure section
        procedure_sections = [s for s in sections.values() if s.type in ['paragraph', 'section']]
        if procedure_sections:
            return procedure_sections[0]
        
        return None


def create_optimized_cobol_content(cobol_content: str, target_entities: List[str] = None, 
                                 max_lines: int = 200) -> str:
    """
    Create optimized COBOL content for AI processing
    
    Args:
        cobol_content: Full COBOL program content
        target_entities: List of entity names to focus on
        max_lines: Maximum number of lines to include
        
    Returns:
        Optimized COBOL content
    """
    extractor = COBOLSectionExtractor()
    sections = extractor.extract_relevant_sections(cobol_content, target_entities)
    return extractor.create_optimized_prompt_content(sections, max_lines)


if __name__ == "__main__":
    print("🧪 COBOL Section Extractor")
    print("   Use: create_optimized_cobol_content(cobol_content, target_entities, max_lines)")
    print("   Optimizes COBOL content for AI processing by extracting relevant sections")
