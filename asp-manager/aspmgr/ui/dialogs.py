"""Dialog components for ASP Manager CUI application."""

import curses
from typing import Optional, List, Dict, Any
from .base import Window
from ..constants import Colors, Keys, Borders


class Dialog(Window):
    """Base dialog window."""
    
    def __init__(self, height: int, width: int, title: str, 
                 parent_height: int, parent_width: int):
        # Center the dialog
        y = (parent_height - height) // 2
        x = (parent_width - width) // 2
        super().__init__(height, width, y, x, title, True, Colors.DIALOG)
        self.result = None
        
    def show_and_wait(self) -> Any:
        """Show dialog and wait for user input."""
        self.show()
        self.set_focus(True)
        
        while True:
            key = self.window.getch()
            if self.handle_key(key):
                break
                
        self.hide()
        return self.result
        
    def handle_key(self, key: int) -> bool:
        """Handle key press. Returns True to close dialog."""
        if key == Keys.ESC:
            self.result = None
            return True
        return False
        

class MessageDialog(Dialog):
    """Simple message dialog."""
    
    def __init__(self, title: str, message: str, 
                 parent_height: int, parent_width: int):
        lines = message.split('\n')
        width = max(len(title) + 4, max(len(line) for line in lines) + 4, 30)
        height = len(lines) + 4
        
        super().__init__(height, width, title, parent_height, parent_width)
        self.message = message
        
    def draw(self):
        """Draw the dialog."""
        super().draw()
        
        if not self.visible:
            return
            
        # Draw message
        lines = self.message.split('\n')
        start_y = 2
        for i, line in enumerate(lines):
            x = (self.width - len(line)) // 2
            self.window.addstr(start_y + i, x, line)
            
        # Draw prompt
        prompt = "Press any key to continue"
        x = (self.width - len(prompt)) // 2
        self.window.attron(curses.A_DIM)
        self.window.addstr(self.height - 2, x, prompt)
        self.window.attroff(curses.A_DIM)
        
        self.refresh()
        
    def handle_key(self, key: int) -> bool:
        """Any key closes the dialog."""
        return True
        

class ConfirmDialog(Dialog):
    """Confirmation dialog with Yes/No options."""
    
    def __init__(self, title: str, message: str,
                 parent_height: int, parent_width: int,
                 default: bool = False):
        lines = message.split('\n')
        width = max(len(title) + 4, max(len(line) for line in lines) + 4, 40)
        height = len(lines) + 5
        
        super().__init__(height, width, title, parent_height, parent_width)
        self.message = message
        self.selected = 1 if default else 0  # 0=No, 1=Yes
        
    def draw(self):
        """Draw the dialog."""
        super().draw()
        
        if not self.visible:
            return
            
        # Draw message
        lines = self.message.split('\n')
        start_y = 2
        for i, line in enumerate(lines):
            x = (self.width - len(line)) // 2
            self.window.addstr(start_y + i, x, line)
            
        # Draw buttons
        button_y = self.height - 2
        button_width = 8
        total_width = button_width * 2 + 3
        start_x = (self.width - total_width) // 2
        
        # No button
        if self.selected == 0:
            self.window.attron(curses.color_pair(Colors.BUTTON_SELECTED))
            self.window.attron(curses.A_REVERSE)
        else:
            self.window.attron(curses.color_pair(Colors.BUTTON))
            
        self.window.addstr(button_y, start_x, "[  No  ]")
        
        if self.selected == 0:
            self.window.attroff(curses.A_REVERSE)
        self.window.attroff(curses.color_pair(Colors.BUTTON) | 
                          curses.color_pair(Colors.BUTTON_SELECTED))
        
        # Yes button
        if self.selected == 1:
            self.window.attron(curses.color_pair(Colors.BUTTON_SELECTED))
            self.window.attron(curses.A_REVERSE)
        else:
            self.window.attron(curses.color_pair(Colors.BUTTON))
            
        self.window.addstr(button_y, start_x + button_width + 3, "[ Yes ]")
        
        if self.selected == 1:
            self.window.attroff(curses.A_REVERSE)
        self.window.attroff(curses.color_pair(Colors.BUTTON) | 
                          curses.color_pair(Colors.BUTTON_SELECTED))
        
        self.refresh()
        
    def handle_key(self, key: int) -> bool:
        """Handle key press."""
        if key == Keys.LEFT:
            self.selected = 0
            self.draw()
        elif key == Keys.RIGHT:
            self.selected = 1
            self.draw()
        elif key == Keys.TAB:
            self.selected = 1 - self.selected
            self.draw()
        elif key == Keys.ENTER or key == Keys.SPACE:
            self.result = self.selected == 1
            return True
        elif key == ord('y') or key == ord('Y'):
            self.result = True
            return True
        elif key == ord('n') or key == ord('N'):
            self.result = False
            return True
        elif key == Keys.ESC:
            self.result = False
            return True
            
        return False
        

class InputDialog(Dialog):
    """Text input dialog."""
    
    def __init__(self, title: str, prompt: str,
                 parent_height: int, parent_width: int,
                 default: str = "", password: bool = False,
                 max_length: int = 255):
        width = max(len(title) + 4, len(prompt) + 4, 50)
        height = 7
        
        super().__init__(height, width, title, parent_height, parent_width)
        self.prompt = prompt
        self.value = default
        self.password = password
        self.max_length = max_length
        self.cursor_pos = len(default)
        
    def draw(self):
        """Draw the dialog."""
        super().draw()
        
        if not self.visible:
            return
            
        # Draw prompt
        self.window.addstr(2, 2, self.prompt)
        
        # Draw input field
        field_y = 4
        field_x = 2
        field_width = self.width - 4
        
        # Draw input border
        self.window.attron(curses.color_pair(Colors.INPUT))
        self.window.addstr(field_y - 1, field_x - 1, '┌' + '─' * field_width + '┐')
        self.window.addstr(field_y, field_x - 1, '│')
        self.window.addstr(field_y, field_x + field_width, '│')
        self.window.addstr(field_y + 1, field_x - 1, '└' + '─' * field_width + '┘')
        
        # Draw input value
        if self.password:
            display_value = '*' * len(self.value)
        else:
            display_value = self.value
            
        # Handle scrolling if value is too long
        if len(display_value) > field_width - 1:
            if self.cursor_pos < field_width - 1:
                display_value = display_value[:field_width - 1]
            else:
                start = self.cursor_pos - field_width + 2
                display_value = display_value[start:start + field_width - 1]
                
        self.window.addstr(field_y, field_x, display_value.ljust(field_width - 1))
        
        # Position cursor
        if self.cursor_pos < field_width - 1:
            cursor_x = field_x + self.cursor_pos
        else:
            cursor_x = field_x + field_width - 2
            
        self.window.move(field_y, cursor_x)
        self.window.attroff(curses.color_pair(Colors.INPUT))
        
        # Draw hint
        hint = "Press Enter to confirm, ESC to cancel"
        x = (self.width - len(hint)) // 2
        self.window.attron(curses.A_DIM)
        self.window.addstr(self.height - 2, x, hint)
        self.window.attroff(curses.A_DIM)
        
        self.refresh()
        
    def handle_key(self, key: int) -> bool:
        """Handle key press."""
        if key == Keys.ENTER:
            self.result = self.value
            return True
        elif key == Keys.ESC:
            self.result = None
            return True
        elif key == curses.KEY_BACKSPACE or key == 127:
            if self.cursor_pos > 0:
                self.value = (self.value[:self.cursor_pos - 1] + 
                             self.value[self.cursor_pos:])
                self.cursor_pos -= 1
                self.draw()
        elif key == Keys.LEFT:
            if self.cursor_pos > 0:
                self.cursor_pos -= 1
                self.draw()
        elif key == Keys.RIGHT:
            if self.cursor_pos < len(self.value):
                self.cursor_pos += 1
                self.draw()
        elif key == Keys.HOME:
            self.cursor_pos = 0
            self.draw()
        elif key == Keys.END:
            self.cursor_pos = len(self.value)
            self.draw()
        elif 32 <= key <= 126:  # Printable characters
            if len(self.value) < self.max_length:
                self.value = (self.value[:self.cursor_pos] + 
                             chr(key) + 
                             self.value[self.cursor_pos:])
                self.cursor_pos += 1
                self.draw()
                
        return False
        

class ListDialog(Dialog):
    """List selection dialog."""
    
    def __init__(self, title: str, prompt: str, items: List[str],
                 parent_height: int, parent_width: int,
                 multi_select: bool = False):
        # Calculate dimensions
        width = max(len(title) + 4, len(prompt) + 4, 
                   max(len(item) for item in items) + 6, 40)
        height = min(len(items) + 5, parent_height - 4)
        
        super().__init__(height, width, title, parent_height, parent_width)
        self.prompt = prompt
        self.items = items
        self.multi_select = multi_select
        self.selected_index = 0
        self.selected_items = set()
        self.scroll_position = 0
        
    def draw(self):
        """Draw the dialog."""
        super().draw()
        
        if not self.visible:
            return
            
        # Draw prompt
        self.window.addstr(1, 2, self.prompt)
        
        # Calculate list area
        list_start_y = 3
        list_height = self.height - 5
        list_width = self.width - 4
        
        # Draw items
        for i in range(list_height):
            item_idx = self.scroll_position + i
            if item_idx >= len(self.items):
                break
                
            item = self.items[item_idx]
            
            # Prepare item text
            if self.multi_select:
                checkbox = "[X]" if item_idx in self.selected_items else "[ ]"
                item_text = f"{checkbox} {item}"
            else:
                item_text = item
                
            # Truncate if too long
            if len(item_text) > list_width:
                item_text = item_text[:list_width - 3] + "..."
                
            # Highlight current item
            if item_idx == self.selected_index:
                self.window.attron(curses.color_pair(Colors.MENU_SELECTED))
                self.window.attron(curses.A_REVERSE)
                
            self.window.addstr(list_start_y + i, 2, item_text.ljust(list_width))
            
            if item_idx == self.selected_index:
                self.window.attroff(curses.A_REVERSE)
                self.window.attroff(curses.color_pair(Colors.MENU_SELECTED))
                
        # Draw scrollbar if needed
        if len(self.items) > list_height:
            self._draw_list_scrollbar(list_start_y, list_height)
            
        # Draw hint
        if self.multi_select:
            hint = "Space: select, Enter: confirm, ESC: cancel"
        else:
            hint = "Enter: select, ESC: cancel"
        x = (self.width - len(hint)) // 2
        self.window.attron(curses.A_DIM)
        self.window.addstr(self.height - 2, x, hint)
        self.window.attroff(curses.A_DIM)
        
        self.refresh()
        
    def _draw_list_scrollbar(self, start_y: int, height: int):
        """Draw scrollbar for the list."""
        x = self.width - 2
        
        # Calculate scrollbar position
        scrollbar_height = max(1, int(height * height / len(self.items)))
        scrollbar_pos = int(self.scroll_position * (height - scrollbar_height) / 
                           (len(self.items) - height))
        
        # Draw scrollbar
        for i in range(height):
            if scrollbar_pos <= i < scrollbar_pos + scrollbar_height:
                self.window.addstr(start_y + i, x, '█')
            else:
                self.window.addstr(start_y + i, x, '│')
                
    def handle_key(self, key: int) -> bool:
        """Handle key press."""
        list_height = self.height - 5
        
        if key == Keys.UP:
            if self.selected_index > 0:
                self.selected_index -= 1
                if self.selected_index < self.scroll_position:
                    self.scroll_position = self.selected_index
            self.draw()
        elif key == Keys.DOWN:
            if self.selected_index < len(self.items) - 1:
                self.selected_index += 1
                if self.selected_index >= self.scroll_position + list_height:
                    self.scroll_position = self.selected_index - list_height + 1
            self.draw()
        elif key == Keys.PAGE_UP:
            self.selected_index = max(0, self.selected_index - list_height)
            self.scroll_position = max(0, self.scroll_position - list_height)
            self.draw()
        elif key == Keys.PAGE_DOWN:
            self.selected_index = min(len(self.items) - 1, 
                                    self.selected_index + list_height)
            max_scroll = max(0, len(self.items) - list_height)
            self.scroll_position = min(max_scroll, 
                                     self.scroll_position + list_height)
            self.draw()
        elif key == Keys.SPACE and self.multi_select:
            if self.selected_index in self.selected_items:
                self.selected_items.remove(self.selected_index)
            else:
                self.selected_items.add(self.selected_index)
            self.draw()
        elif key == Keys.ENTER:
            if self.multi_select:
                self.result = [self.items[i] for i in sorted(self.selected_items)]
            else:
                self.result = self.items[self.selected_index]
            return True
        elif key == Keys.ESC:
            self.result = None
            return True
            
        return False