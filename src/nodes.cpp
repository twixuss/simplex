#include "nodes.h"

bool is_substitutable(Block *block) {
	return block->children.count == 1 && block->breaks.count == 0;
}

#if ENABLE_NOTE_LEAK

GList<String> leaks;

void note_leak(String expression, Node *node, String message = {}, std::source_location location = std::source_location::current()) {
	if (message)
		leaks.add(format(u8"{} ({}) at {}:{} - {}. {}", expression, node->kind, location.file_name(), location.line(), location.function_name(), message));
	else 
		leaks.add(format(u8"{} ({}) at {}:{} - {}", expression, node->kind, location.file_name(), location.line(), location.function_name()));
}

#endif