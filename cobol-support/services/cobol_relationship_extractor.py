"""
COBOL Relationship Extractor
Extracts advanced COBOL relationships from parsed COBOL data
"""

from typing import Dict, List, Any, Optional
import sys
import os

# Add the project root to the path
project_root = os.path.abspath(os.path.join(os.path.dirname(__file__), '..', '..'))
if project_root not in sys.path:
    sys.path.insert(0, project_root)

try:
    from ai_services.models.relationship_models import RelationshipType, RelationshipExtraction
except ImportError:
    # Fallback: define the classes locally if import fails
    from enum import Enum
    from dataclasses import dataclass
    from typing import Optional, Dict, Any
    
    class RelationshipType(Enum):
        CALLS = "CALLS"
        INHERITS = "INHERITS"
        USES = "USES"
        IMPLEMENTS = "IMPLEMENTS"
        DEPENDS_ON = "DEPENDS_ON"
        OVERRIDES = "OVERRIDES"
        DECORATES = "DECORATES"
        INSTANTIATES = "INSTANTIATES"
        DEFINES = "DEFINES"
        CONFIGURES = "CONFIGURES"
        TRANSFORMS = "TRANSFORMS"
        VALIDATES = "VALIDATES"
        DATA_FLOW = "DATA_FLOW"
        STATE_MUTATION = "STATE_MUTATION"
        IMPORTS = "IMPORTS"
        EXPORTS = "EXPORTS"
        EVENT_HANDLING = "EVENT_HANDLING"
        INCLUDES = "INCLUDES"
        PASSES_DATA = "PASSES_DATA"
        HANDLES_ERRORS = "HANDLES_ERRORS"
        USES_QUEUE = "USES_QUEUE"
        BINDS_SCREEN = "BINDS_SCREEN"
        PERFORMS = "PERFORMS"
        REPLACES = "REPLACES"
        CONTAINS = "CONTAINS"
    
    @dataclass
    class RelationshipExtraction:
        source_file: str
        target_file: str
        source_entity: str
        target_entity: str
        relationship_type: RelationshipType
        confidence: float
        relationship_strength: str = "medium"
        line_number: int = 0
        context: str = ""
        metadata: Optional[Dict[str, Any]] = None


class COBOLRelationshipExtractor:
    """Extracts COBOL-specific relationships from parsed data"""
    
    def _create_relationship(self, cobol_data: Dict[str, Any], source_entity: str, target_entity: str, 
                           relationship_type: RelationshipType, confidence: float = 0.8, 
                           context: str = "", metadata: Optional[Dict[str, Any]] = None) -> RelationshipExtraction:
        """Helper method to create RelationshipExtraction objects with consistent parameters"""
        return RelationshipExtraction(
            source_file=cobol_data.get("file_path", ""),
            target_file=cobol_data.get("file_path", ""),
            source_entity=source_entity,
            target_entity=target_entity,
            relationship_type=relationship_type,
            confidence=confidence,
            context=context,
            metadata=metadata or {}
        )
    
    def extract_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract COBOL relationships from parsed data"""
        relationships = []
        
        # Extract COPY statement relationships (INCLUDES)
        relationships.extend(self._extract_copy_relationships(cobol_data))
        
        # Extract CALL statement relationships (CALLS)
        relationships.extend(self._extract_call_relationships(cobol_data))
        
        # Extract parameter passing relationships (PASSES_DATA)
        relationships.extend(self._extract_parameter_relationships(cobol_data))
        
        # Extract PERFORM statement relationships (PERFORMS)
        relationships.extend(self._extract_perform_relationships(cobol_data))
        
        # Extract data flow relationships (DATA_FLOW)
        relationships.extend(self._extract_data_flow_relationships(cobol_data))
        
        # Extract paragraph control flow relationships (PERFORMS)
        relationships.extend(self._extract_paragraph_relationships(cobol_data))
        
        # Extract USE statement relationships (HANDLES_ERRORS)
        relationships.extend(self._extract_use_relationships(cobol_data))
        
        # Extract communication relationships (USES_QUEUE)
        relationships.extend(self._extract_communication_relationships(cobol_data))
        
        # Extract screen relationships (BINDS_SCREEN)
        relationships.extend(self._extract_screen_relationships(cobol_data))
        
        # Extract containment relationships (CONTAINS) - connect fine-grained entities to their parents
        relationships.extend(self._extract_containment_relationships(cobol_data))
        
        return relationships
    
    def _extract_copy_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract COPY statement relationships"""
        relationships = []
        
        copy_statements = cobol_data.get('copy_statements', {})
        for unit_name, copies in copy_statements.items():
            for copy_info in copies:
                copy_name = copy_info['name']
                copy_library = copy_info.get('library', '')
                
                # Create INCLUDES relationship from program to copybook
                relationships.append(RelationshipExtraction(
                    source_file=cobol_data.get("file_path", ""),
                    target_file=cobol_data.get("file_path", ""),
                    source_entity=f"PROGRAM:{unit_name}",
                    target_entity=f"COPYBOOK:{copy_name}",
                    relationship_type=RelationshipType.INCLUDES,
                    confidence=0.9,
                    context=f"COPY statement includes {copy_name} from {copy_library}" if copy_library else f"COPY statement includes {copy_name}",
                    metadata={
                        'copy_library': copy_library,
                        'unit_name': unit_name,
                        'source_type': 'cobol_program',
                        'target_type': 'cobol_copybook'
                    }
                ))
        
        # Extract REPLACING relationships
        replacing_phrases = cobol_data.get('replacing_phrases', {})
        for unit_name, copy_replacements in replacing_phrases.items():
            for copy_name, replacements in copy_replacements.items():
                for replacement in replacements:
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"COPYBOOK:{copy_name}",
                        target_entity=f"REPLACEMENT:{replacement['replacement']}",
                        relationship_type=RelationshipType.REPLACES,
                        confidence=0.8,
                        context=f"REPLACING phrase replaces {replacement['replaceable']} with {replacement['replacement']}",
                        metadata={
                            'replaceable': replacement['replaceable'],
                            'replacement': replacement['replacement'],
                            'copy_name': copy_name,
                            'unit_name': unit_name
                        }
                    ))
        
        return relationships
    
    def _extract_call_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract CALL statement relationships from parsed statements"""
        relationships = []
        
        # Get the compilation unit name
        unit_name = cobol_data.get("compilation_units", [{}])[0].get("name", "UNKNOWN")
        
        # Extract CALL relationships from statements
        statements = cobol_data.get("statements", {}).get(unit_name, {})
        for para_name, para_statements in statements.items():
            for stmt in para_statements:
                if isinstance(stmt, dict) and stmt.get("type") == "CallStatementImpl":
                    # Extract program name from the details
                    details = stmt.get("details", "")
                    if "CALL" in details:
                        # Parse the CALL statement details
                        call_parts = details.split("CALL")[1].split("USING")[0].strip()
                        program_name = call_parts.strip("'\"")
                        
                        # Create CALLS relationship
                        relationships.append(self._create_relationship(
                            cobol_data,
                            source_entity=f"PROGRAM:{unit_name}",
                            target_entity=f"PROGRAM:{program_name}",
                            relationship_type=RelationshipType.CALLS,
                            confidence=0.9,
                            context=f"CALL statement in paragraph {para_name} calls {program_name}",
                            metadata={
                                'paragraph_name': para_name,
                                'unit_name': unit_name,
                                'source_type': 'cobol_program',
                                'target_type': 'cobol_program',
                                'call_details': details
                            }
                        ))
        
        return relationships
    
    def _extract_parameter_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract parameter passing relationships"""
        relationships = []
        
        call_parameters = cobol_data.get('call_parameters', {})
        for unit_name, para_params in call_parameters.items():
            for para_name, params in para_params.items():
                for param_info in params:
                    program_name = param_info['program_name']
                    param_type = param_info['param_type']
                    param_name = param_info['param_name']
                    
                    # Create PASSES_DATA relationship
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"DATA_ITEM:{param_name}",
                        target_entity=f"PROGRAM:{program_name}",
                        relationship_type=RelationshipType.PASSES_DATA,
                        confidence=0.8,
                        context=f"Parameter {param_name} passed {param_type} to {program_name}",
                        metadata={
                            'parameter_type': param_type,
                            'parameter_name': param_name,
                            'program_name': program_name,
                            'paragraph_name': para_name,
                            'unit_name': unit_name
                        }
                    ))
        
        # Extract GIVING relationships
        call_giving = cobol_data.get('call_giving', {})
        for unit_name, para_giving in call_giving.items():
            for para_name, giving_list in para_giving.items():
                for giving_info in giving_list:
                    program_name = giving_info['program_name']
                    giving_param = giving_info['giving_param']
                    
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"PROGRAM:{program_name}",
                        target_entity=f"DATA_ITEM:{giving_param}",
                        relationship_type=RelationshipType.PASSES_DATA,
                        confidence=0.8,
                        context=f"Program {program_name} returns data to {giving_param}",
                        metadata={
                            'giving_parameter': giving_param,
                            'program_name': program_name,
                            'paragraph_name': para_name,
                            'unit_name': unit_name
                        }
                    ))
        
        return relationships
    
    def _extract_data_flow_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract data flow relationships from MOVE statements and data operations"""
        relationships = []
        
        # Get the compilation unit name
        unit_name = cobol_data.get("compilation_units", [{}])[0].get("name", "UNKNOWN")
        
        # Get list of actual data items from entities
        actual_data_items = set()
        entities = cobol_data.get("entities", [])
        for entity in entities:
            if entity.get("type") == "data_item":
                actual_data_items.add(entity.get("name", ""))
        
        
        # Extract data flow relationships from statements
        statements = cobol_data.get("statements", {}).get(unit_name, {})
        for para_name, para_statements in statements.items():
            for stmt in para_statements:
                if isinstance(stmt, dict) and stmt.get("type") == "MoveStatementImpl":
                    details = stmt.get("details", "")
                    if "MOVE" in details:
                        # Parse MOVE statement: MOVE source TO destination
                        move_parts = details.split("MOVE")[1].strip()
                        if "TO" in move_parts:
                            source_dest = move_parts.split("TO")
                            if len(source_dest) >= 2:
                                source = source_dest[0].strip()
                                destination = source_dest[1].strip()
                                
                                # Only create relationships for actual data items (not literals)
                                if self._is_actual_data_item(source, actual_data_items) and self._is_actual_data_item(destination, actual_data_items):
                                    # Create DATA_FLOW relationship between data items
                                    relationships.append(self._create_relationship(
                                        cobol_data,
                                        source_entity=f"DATA_ITEM:{source}",
                                        target_entity=f"DATA_ITEM:{destination}",
                                        relationship_type=RelationshipType.DATA_FLOW,
                                        confidence=0.8,
                                        context=f"MOVE statement transfers data from {source} to {destination}",
                                        metadata={
                                            'source_data_item': source,
                                            'destination_data_item': destination,
                                            'paragraph_name': para_name,
                                            'unit_name': unit_name,
                                            'move_details': details,
                                            'source_type': 'cobol_data_item',
                                            'target_type': 'cobol_data_item'
                                        }
                                    ))
                                    
                                    # Create USES relationship from paragraph to source data item
                                    relationships.append(self._create_relationship(
                                        cobol_data,
                                        source_entity=f"PARAGRAPH:{para_name}",
                                        target_entity=f"DATA_ITEM:{source}",
                                        relationship_type=RelationshipType.USES,
                                        confidence=0.7,
                                        context=f"Paragraph {para_name} reads from data item {source}",
                                        metadata={
                                            'paragraph_name': para_name,
                                            'data_item': source,
                                            'unit_name': unit_name,
                                            'operation': 'read',
                                            'source_type': 'cobol_paragraph',
                                            'target_type': 'cobol_data_item'
                                        }
                                    ))
                                    
                                    # Create USES relationship from paragraph to destination data item
                                    relationships.append(self._create_relationship(
                                        cobol_data,
                                        source_entity=f"PARAGRAPH:{para_name}",
                                        target_entity=f"DATA_ITEM:{destination}",
                                        relationship_type=RelationshipType.USES,
                                        confidence=0.7,
                                        context=f"Paragraph {para_name} writes to data item {destination}",
                                        metadata={
                                            'paragraph_name': para_name,
                                            'data_item': destination,
                                            'unit_name': unit_name,
                                            'operation': 'write',
                                            'source_type': 'cobol_paragraph',
                                            'target_type': 'cobol_data_item'
                                        }
                                    ))
                                    
                                    # Create USES relationship from program to data items
                                    relationships.append(self._create_relationship(
                                        cobol_data,
                                        source_entity=f"PROGRAM:{unit_name}",
                                        target_entity=f"DATA_ITEM:{source}",
                                        relationship_type=RelationshipType.USES,
                                        confidence=0.6,
                                        context=f"Program {unit_name} uses data item {source}",
                                        metadata={
                                            'program_name': unit_name,
                                            'data_item': source,
                                            'paragraph_name': para_name,
                                            'operation': 'read',
                                            'source_type': 'cobol_program',
                                            'target_type': 'cobol_data_item'
                                        }
                                    ))
                                    
                                    relationships.append(self._create_relationship(
                                        cobol_data,
                                        source_entity=f"PROGRAM:{unit_name}",
                                        target_entity=f"DATA_ITEM:{destination}",
                                        relationship_type=RelationshipType.USES,
                                        confidence=0.6,
                                        context=f"Program {unit_name} uses data item {destination}",
                                        metadata={
                                            'program_name': unit_name,
                                            'data_item': destination,
                                            'paragraph_name': para_name,
                                            'operation': 'write',
                                            'source_type': 'cobol_program',
                                            'target_type': 'cobol_data_item'
                                        }
                                    ))
        
        return relationships
    
    def _is_actual_data_item(self, name: str, actual_data_items: set) -> bool:
        """Check if a name is an actual data item (not a literal value)"""
        # Remove quotes and check if it's in the actual data items set
        clean_name = name.strip("'\"")
        
        # Skip obvious literal values (quoted strings, numbers, special values)
        if (clean_name.startswith("'") and clean_name.endswith("'")) or \
           (clean_name.startswith('"') and clean_name.endswith('"')) or \
           clean_name.upper() in ['SPACES', 'ZERO', 'ZEROS', 'HIGH-VALUES', 'LOW-VALUES', 'NULL', 'NULLS'] or \
           clean_name.isdigit():
            return False
        
        # Skip single character literals that are clearly not data items
        if len(clean_name) == 1 and clean_name in ['A', 'B', 'C', 'D', 'W', 'T']:
            return False
        
        # Skip short literal strings that are clearly not data items
        if clean_name in ['INIT', 'DEP', 'WTH', 'TRF', 'CASH', 'ATM', 'ONLINE', 'TELLER', 'OPENING']:
            return False
        
        # Check if it's in the actual data items set
        return clean_name in actual_data_items
    
    def _extract_paragraph_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract paragraph control flow relationships"""
        relationships = []
        
        # Get the compilation unit name
        unit_name = cobol_data.get("compilation_units", [{}])[0].get("name", "UNKNOWN")
        
        # Get all paragraphs
        paragraphs = cobol_data.get("paragraphs", {}).get(unit_name, [])
        para_names = [para.get("name", "") for para in paragraphs if isinstance(para, dict)]
        
        # Create hierarchical relationships between paragraphs
        # Main logic paragraph calls other paragraphs
        main_para = "0000-MAIN-LOGIC"
        if main_para in para_names:
            for para in para_names:
                if para != main_para and para.startswith(("1000-", "2000-", "3000-", "4000-", "5000-", "6000-", "7000-", "8000-", "9000-")):
                    # Create hierarchical relationship
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"PARAGRAPH:{main_para}",
                        target_entity=f"PARAGRAPH:{para}",
                        relationship_type=RelationshipType.PERFORMS,
                        confidence=0.7,
                        context=f"Main logic paragraph {main_para} orchestrates {para}",
                        metadata={
                            'source_paragraph': main_para,
                            'target_paragraph': para,
                            'unit_name': unit_name,
                            'source_type': 'cobol_paragraph',
                            'target_type': 'cobol_paragraph',
                            'relationship_type': 'hierarchical'
                        }
                    ))
        
        return relationships
    
    def _extract_use_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract USE statement relationships"""
        relationships = []
        
        use_statements = cobol_data.get('use_statements', {})
        for unit_name, uses in use_statements.items():
            for use_info in uses:
                use_type = use_info['use_type']
                file_name = use_info.get('file_name', '')
                procedure_name = use_info.get('procedure_name', '')
                
                if file_name:
                    # Create HANDLES_ERRORS relationship for file
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"PROGRAM:{unit_name}",
                        target_entity=f"FILE:{file_name}",
                        relationship_type=RelationshipType.HANDLES_ERRORS,
                        confidence=0.9,
                        context=f"USE statement handles errors for {use_type} on file {file_name}",
                        metadata={
                            'use_type': use_type,
                            'file_name': file_name,
                            'unit_name': unit_name
                        }
                    ))
                
                if procedure_name:
                    # Create HANDLES_ERRORS relationship for procedure
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"PROGRAM:{unit_name}",
                        target_entity=f"PROCEDURE:{procedure_name}",
                        relationship_type=RelationshipType.HANDLES_ERRORS,
                        confidence=0.9,
                        context=f"USE statement handles errors for {use_type} in procedure {procedure_name}",
                        metadata={
                            'use_type': use_type,
                            'procedure_name': procedure_name,
                            'unit_name': unit_name
                        }
                    ))
        
        return relationships
    
    def _extract_communication_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract communication relationships"""
        relationships = []
        
        communication = cobol_data.get('communication', {})
        for unit_name, comm_list in communication.items():
            for comm_info in comm_list:
                comm_name = comm_info['name']
                comm_type = comm_info['type']
                symbolic_queue = comm_info.get('symbolic_queue', '')
                symbolic_destination = comm_info.get('symbolic_destination', '')
                
                if symbolic_queue:
                    # Create USES_QUEUE relationship
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"PROGRAM:{unit_name}",
                        target_entity=f"QUEUE:{symbolic_queue}",
                        relationship_type=RelationshipType.USES_QUEUE,
                        confidence=0.8,
                        context=f"Communication {comm_name} uses queue {symbolic_queue}",
                        metadata={
                            'communication_name': comm_name,
                            'communication_type': comm_type,
                            'queue_name': symbolic_queue,
                            'unit_name': unit_name
                        }
                    ))
                
                if symbolic_destination:
                    # Create USES_QUEUE relationship for destination
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"PROGRAM:{unit_name}",
                        target_entity=f"DESTINATION:{symbolic_destination}",
                        relationship_type=RelationshipType.USES_QUEUE,
                        confidence=0.8,
                        context=f"Communication {comm_name} uses destination {symbolic_destination}",
                        metadata={
                            'communication_name': comm_name,
                            'communication_type': comm_type,
                            'destination_name': symbolic_destination,
                            'unit_name': unit_name
                        }
                    ))
        
        return relationships
    
    def _extract_screen_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract screen relationships"""
        relationships = []
        
        screens = cobol_data.get('screens', {})
        for unit_name, screen_list in screens.items():
            for screen_info in screen_list:
                screen_name = screen_info['name']
                screen_value = screen_info.get('value', '')
                screen_from = screen_info.get('from', '')
                screen_to = screen_info.get('to', '')
                
                if screen_from:
                    # Create BINDS_SCREEN relationship for FROM field
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"SCREEN:{screen_name}",
                        target_entity=f"DATA_ITEM:{screen_from}",
                        relationship_type=RelationshipType.BINDS_SCREEN,
                        confidence=0.9,
                        context=f"Screen {screen_name} binds to data item {screen_from}",
                        metadata={
                            'screen_name': screen_name,
                            'data_item': screen_from,
                            'binding_type': 'FROM',
                            'unit_name': unit_name
                        }
                    ))
                
                if screen_to:
                    # Create BINDS_SCREEN relationship for TO field
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"SCREEN:{screen_name}",
                        target_entity=f"DATA_ITEM:{screen_to}",
                        relationship_type=RelationshipType.BINDS_SCREEN,
                        confidence=0.9,
                        context=f"Screen {screen_name} binds to data item {screen_to}",
                        metadata={
                            'screen_name': screen_name,
                            'data_item': screen_to,
                            'binding_type': 'TO',
                            'unit_name': unit_name
                        }
                    ))
        
        return relationships
    
    def _extract_perform_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract PERFORM statement relationships from parsed statements"""
        relationships = []
        
        # Get the compilation unit name
        unit_name = cobol_data.get("compilation_units", [{}])[0].get("name", "UNKNOWN")
        
        # Extract PERFORM relationships from statements
        statements = cobol_data.get("statements", {}).get(unit_name, {})
        for para_name, para_statements in statements.items():
            for stmt in para_statements:
                if isinstance(stmt, dict) and stmt.get("type") == "PerformStatementImpl":
                    details = stmt.get("details", "")
                    if "PERFORM" in details:
                        # Extract target paragraph name
                        perform_parts = details.split("PERFORM")[1].strip()
                        target_para = perform_parts.split()[0] if perform_parts.split() else ""
                        
                        if target_para:
                            # Create PERFORMS relationship
                            relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"PARAGRAPH:{para_name}",
                                target_entity=f"PARAGRAPH:{target_para}",
                                relationship_type=RelationshipType.PERFORMS,
                                confidence=0.9,
                                context=f"PERFORM statement in {para_name} calls {target_para}",
                                metadata={
                                    'source_paragraph': para_name,
                                    'target_paragraph': target_para,
                                    'unit_name': unit_name,
                                    'perform_details': details,
                                    'source_type': 'cobol_paragraph',
                                    'target_type': 'cobol_paragraph'
                                }
                            ))
        
        return relationships
    
    def _extract_containment_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract containment relationships to connect fine-grained entities to their parent nodes"""
        relationships = []
        
        # Get the file path and program name
        file_path = cobol_data.get("file_path", "")
        program_name = cobol_data.get("program_name", "UNKNOWN")
        
        # Connect paragraphs to their program
        paragraphs = cobol_data.get('paragraphs', {})
        for unit_name, para_list in paragraphs.items():
            for para_data in para_list:
                # Handle both string and dict paragraph data
                if isinstance(para_data, dict):
                    para_name = para_data.get('name', '')
                else:
                    para_name = str(para_data)
                
                if para_name:
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"PROGRAM:{unit_name}",
                        target_entity=f"PARAGRAPH:{para_name}",
                        relationship_type=RelationshipType.CONTAINS,
                        confidence=1.0,
                        context=f"Program {unit_name} contains paragraph {para_name}",
                        metadata={
                            'unit_name': unit_name,
                            'paragraph_name': para_name,
                            'source_type': 'cobol_program',
                            'target_type': 'cobol_paragraph'
                        }
                    ))
        
        # Connect data items to their program
        data_items = cobol_data.get('data_items', {})
        for unit_name, items in data_items.items():
            for item in items:
                item_name = item.get('name', '')
                if item_name:
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"PROGRAM:{unit_name}",
                        target_entity=f"DATA_ITEM:{item_name}",
                        relationship_type=RelationshipType.CONTAINS,
                        confidence=1.0,
                        context=f"Program {unit_name} contains data item {item_name}",
                        metadata={
                            'unit_name': unit_name,
                            'data_item_name': item_name,
                            'source_type': 'cobol_program',
                            'target_type': 'cobol_data_item'
                        }
                    ))
        
        # Connect screens to their program
        screens = cobol_data.get('screens', {})
        for unit_name, screen_list in screens.items():
            for screen in screen_list:
                screen_name = screen.get('name', '')
                if screen_name:
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"PROGRAM:{unit_name}",
                        target_entity=f"SCREEN:{screen_name}",
                        relationship_type=RelationshipType.CONTAINS,
                        confidence=1.0,
                        context=f"Program {unit_name} contains screen {screen_name}",
                        metadata={
                            'unit_name': unit_name,
                            'screen_name': screen_name,
                            'source_type': 'cobol_program',
                            'target_type': 'cobol_screen'
                        }
                    ))
        
        # Connect queues to their program
        queues = cobol_data.get('queues', {})
        for unit_name, queue_list in queues.items():
            for queue in queue_list:
                queue_name = queue.get('name', '')
                if queue_name:
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"PROGRAM:{unit_name}",
                        target_entity=f"QUEUE:{queue_name}",
                        relationship_type=RelationshipType.CONTAINS,
                        confidence=1.0,
                        context=f"Program {unit_name} contains queue {queue_name}",
                        metadata={
                            'unit_name': unit_name,
                            'queue_name': queue_name,
                            'source_type': 'cobol_program',
                            'target_type': 'cobol_queue'
                        }
                    ))
        
        # Connect file to programs and compilation units
        if file_path:
            # Create file -> program relationships
            compilation_units = cobol_data.get('compilation_units', [])
            for unit in compilation_units:
                unit_name = unit.get('name', '') if isinstance(unit, dict) else str(unit)
                if unit_name:
                    # File -> Program relationship
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"FILE:{file_path}",
                        target_entity=f"PROGRAM:{unit_name}",
                        relationship_type=RelationshipType.CONTAINS,
                        confidence=1.0,
                        context=f"File {file_path} contains program {unit_name}",
                        metadata={
                            'file_path': file_path,
                            'program_name': unit_name,
                            'source_type': 'cobol_file',
                            'target_type': 'cobol_program'
                        }
                    ))
                    
                    # File -> Compilation Unit relationship
                    relationships.append(self._create_relationship(
                        cobol_data,
                        source_entity=f"FILE:{file_path}",
                        target_entity=f"COMPILATION_UNIT:{unit_name}",
                        relationship_type=RelationshipType.CONTAINS,
                        confidence=1.0,
                        context=f"File {file_path} contains compilation unit {unit_name}",
                        metadata={
                            'file_path': file_path,
                            'compilation_unit_name': unit_name,
                            'source_type': 'cobol_file',
                            'target_type': 'cobol_compilation_unit'
                        }
                    ))
        
        return relationships
