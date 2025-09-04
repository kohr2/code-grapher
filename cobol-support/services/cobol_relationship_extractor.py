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

from ai_services.models.relationship_models import RelationshipType, RelationshipExtraction


class COBOLRelationshipExtractor:
    """Extracts COBOL-specific relationships from parsed data"""
    
    def extract_relationships(self, cobol_data: Dict[str, Any]) -> List[RelationshipExtraction]:
        """Extract COBOL relationships from parsed data"""
        relationships = []
        
        # Extract COPY statement relationships (INCLUDES)
        relationships.extend(self._extract_copy_relationships(cobol_data))
        
        # Extract CALL statement relationships (CALLS)
        relationships.extend(self._extract_call_relationships(cobol_data))
        
        # Extract parameter passing relationships (PASSES_DATA)
        relationships.extend(self._extract_parameter_relationships(cobol_data))
        
        # Extract USE statement relationships (HANDLES_ERRORS)
        relationships.extend(self._extract_use_relationships(cobol_data))
        
        # Extract communication relationships (USES_QUEUE)
        relationships.extend(self._extract_communication_relationships(cobol_data))
        
        # Extract screen relationships (BINDS_SCREEN)
        relationships.extend(self._extract_screen_relationships(cobol_data))
        
        # Extract PERFORM relationships (PERFORMS)
        relationships.extend(self._extract_perform_relationships(cobol_data))
        
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
                    relationships.append(RelationshipExtraction(
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
        """Extract CALL statement relationships"""
        relationships = []
        
        call_statements = cobol_data.get('call_statements', {})
        for unit_name, para_calls in call_statements.items():
            for para_name, calls in para_calls.items():
                for call_info in calls:
                    program_name = call_info['program_name']
                    
                    # Create CALLS relationship
                    relationships.append(RelationshipExtraction(
                        source_entity=f"PROGRAM:{unit_name}",
                        target_entity=f"PROGRAM:{program_name}",
                        relationship_type=RelationshipType.CALLS,
                        confidence=0.9,
                        context=f"CALL statement in paragraph {para_name} calls {program_name}",
                        metadata={
                            'paragraph_name': para_name,
                            'unit_name': unit_name,
                            'source_type': 'cobol_program',
                            'target_type': 'cobol_program'
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
                    relationships.append(RelationshipExtraction(
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
                    
                    relationships.append(RelationshipExtraction(
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
                    relationships.append(RelationshipExtraction(
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
                    relationships.append(RelationshipExtraction(
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
                    relationships.append(RelationshipExtraction(
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
                    relationships.append(RelationshipExtraction(
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
                    relationships.append(RelationshipExtraction(
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
                    relationships.append(RelationshipExtraction(
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
        """Extract PERFORM statement relationships"""
        relationships = []
        
        statements = cobol_data.get('statements', {})
        for unit_name, para_statements in statements.items():
            for para_name, stmt_list in para_statements.items():
                for stmt_info in stmt_list:
                    if isinstance(stmt_info, str) and 'PERFORM' in stmt_info.upper():
                        # Extract PERFORM target from statement text
                        perform_text = stmt_info.upper()
                        if 'PERFORM' in perform_text:
                            # Simple extraction - look for paragraph names after PERFORM
                            parts = perform_text.split('PERFORM')
                            if len(parts) > 1:
                                target_part = parts[1].strip()
                                # Extract paragraph name (simplified)
                                target_para = target_part.split()[0] if target_part.split() else None
                                if target_para:
                                    relationships.append(RelationshipExtraction(
                                        source_entity=f"PARAGRAPH:{para_name}",
                                        target_entity=f"PARAGRAPH:{target_para}",
                                        relationship_type=RelationshipType.PERFORMS,
                                        confidence=0.7,
                                        context=f"PERFORM statement in {para_name} calls {target_para}",
                                        metadata={
                                            'source_paragraph': para_name,
                                            'target_paragraph': target_para,
                                            'unit_name': unit_name,
                                            'statement_text': stmt_info
                                        }
                                    ))
        
        return relationships
