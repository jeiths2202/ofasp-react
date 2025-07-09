"""Base UI components for ASP Manager CUI application."""

import curses
from typing import Optional, Tuple, List, Callable
from ..constants import Colors, Borders, Keys


class Window:
    """Base window class for all UI components."""
    
    def __init__(self, height: int, width: int, y: int, x: int, 
                 title: Optional[str] = None, 
                 border: bool = True,
                 color_pair: int = Colors.DEFAULT):
        """Initialize a window.
        
        Args:
            height: Window height
            width: Window width  
            y: Y position
            x: X position
            title: Optional window title
            border: Whether to draw border
            color_pair: Color pair to use
        """
        self.height = height
        self.width = width
        self.y = y
        self.x = x
        self.title = title
        self.border = border
        self.color_pair = color_pair
        self.window = curses.newwin(height, width, y, x)
        self.focused = False
        self.visible = True
        
    def draw(self):
        """Draw the window."""
        if not self.visible:
            return
            
        self.window.clear()
        self.window.attron(curses.color_pair(self.color_pair))
        
        if self.border:
            self._draw_border()
            
        if self.title:
            self._draw_title()
            
        self.window.attroff(curses.color_pair(self.color_pair))
        self.refresh()
        
    def _draw_border(self):
        """Draw window border."""
        # Use different border style for focused windows
        if self.focused:
            h_char = Borders.DOUBLE_HORIZONTAL
            v_char = Borders.DOUBLE_VERTICAL
            tl = Borders.DOUBLE_TOP_LEFT
            tr = Borders.DOUBLE_TOP_RIGHT
            bl = Borders.DOUBLE_BOTTOM_LEFT
            br = Borders.DOUBLE_BOTTOM_RIGHT
        else:
            h_char = Borders.SINGLE_HORIZONTAL
            v_char = Borders.SINGLE_VERTICAL
            tl = Borders.SINGLE_TOP_LEFT
            tr = Borders.SINGLE_TOP_RIGHT
            bl = Borders.SINGLE_BOTTOM_LEFT
            br = Borders.SINGLE_BOTTOM_RIGHT
            
        # Top border
        self.window.addstr(0, 0, tl)
        for i in range(1, self.width - 1):
            self.window.addstr(0, i, h_char)
        self.window.addstr(0, self.width - 1, tr)
        
        # Side borders
        for i in range(1, self.height - 1):
            self.window.addstr(i, 0, v_char)
            self.window.addstr(i, self.width - 1, v_char)
            
        # Bottom border
        self.window.addstr(self.height - 1, 0, bl)
        for i in range(1, self.width - 1):
            self.window.addstr(self.height - 1, i, h_char)
        self.window.addstr(self.height - 1, self.width - 1, br)
        
    def _draw_title(self):
        """Draw window title."""
        if not self.title or not self.border:
            return
            
        title_str = f" {self.title} "
        title_x = (self.width - len(title_str)) // 2
        if title_x > 0:
            self.window.attron(curses.A_BOLD)
            self.window.addstr(0, title_x, title_str)
            self.window.attroff(curses.A_BOLD)
            
    def refresh(self):
        """Refresh the window."""
        self.window.refresh()
        
    def clear(self):
        """Clear the window."""
        self.window.clear()
        
    def set_focus(self, focused: bool):
        """Set window focus state."""
        self.focused = focused
        self.draw()
        
    def show(self):
        """Show the window."""
        self.visible = True
        self.draw()
        
    def hide(self):
        """Hide the window."""
        self.visible = False
        self.clear()
        self.refresh()
        
    def move(self, y: int, x: int):
        """Move the window to new position."""
        self.y = y
        self.x = x
        self.window.mvwin(y, x)
        
    def resize(self, height: int, width: int):
        """Resize the window."""
        self.height = height
        self.width = width
        self.window.resize(height, width)
        

class ScrollableWindow(Window):
    """Window with scrolling support."""
    
    def __init__(self, height: int, width: int, y: int, x: int,
                 title: Optional[str] = None, 
                 border: bool = True,
                 color_pair: int = Colors.DEFAULT):
        super().__init__(height, width, y, x, title, border, color_pair)
        self.content: List[str] = []
        self.scroll_position = 0
        self.selected_line = 0
        
    def set_content(self, content: List[str]):
        """Set the content to display."""
        self.content = content
        self.scroll_position = 0
        self.selected_line = 0
        
    def add_line(self, line: str):
        """Add a line to the content."""
        self.content.append(line)
        
    def clear_content(self):
        """Clear all content."""
        self.content = []
        self.scroll_position = 0
        self.selected_line = 0
        
    def draw(self):
        """Draw the window with scrollable content."""
        super().draw()
        
        if not self.visible or not self.content:
            return
            
        # Calculate viewable area
        start_y = 1 if self.border else 0
        start_x = 1 if self.border else 0
        viewable_height = self.height - (2 if self.border else 0)
        viewable_width = self.width - (2 if self.border else 0)
        
        # Draw content
        for i in range(viewable_height):
            line_idx = self.scroll_position + i
            if line_idx >= len(self.content):
                break
                
            line = self.content[line_idx][:viewable_width]
            
            # Highlight selected line
            if line_idx == self.selected_line:
                self.window.attron(curses.color_pair(Colors.HIGHLIGHT))
                self.window.attron(curses.A_REVERSE)
                
            self.window.addstr(start_y + i, start_x, line.ljust(viewable_width))
            
            if line_idx == self.selected_line:
                self.window.attroff(curses.A_REVERSE)
                self.window.attroff(curses.color_pair(Colors.HIGHLIGHT))
                
        # Draw scrollbar if needed
        if len(self.content) > viewable_height:
            self._draw_scrollbar()
            
        self.refresh()
        
    def _draw_scrollbar(self):
        """Draw a scrollbar on the right side."""
        if not self.border:
            return
            
        viewable_height = self.height - 2
        content_height = len(self.content)
        
        # Calculate scrollbar position and size
        scrollbar_height = max(1, int(viewable_height * viewable_height / content_height))
        scrollbar_pos = int(self.scroll_position * (viewable_height - scrollbar_height) / 
                           (content_height - viewable_height))
        
        # Draw scrollbar track
        x = self.width - 2
        for i in range(1, self.height - 1):
            self.window.addstr(i, x, '│')
            
        # Draw scrollbar thumb
        for i in range(scrollbar_height):
            y = 1 + scrollbar_pos + i
            if y < self.height - 1:
                self.window.addstr(y, x, '█')
                
    def scroll_up(self, lines: int = 1):
        """Scroll up by specified lines."""
        self.scroll_position = max(0, self.scroll_position - lines)
        self.selected_line = max(self.scroll_position, 
                                min(self.selected_line, 
                                    self.scroll_position + self._viewable_height() - 1))
        
    def scroll_down(self, lines: int = 1):
        """Scroll down by specified lines."""
        max_scroll = max(0, len(self.content) - self._viewable_height())
        self.scroll_position = min(max_scroll, self.scroll_position + lines)
        self.selected_line = max(self.scroll_position,
                                min(self.selected_line, len(self.content) - 1))
        
    def select_previous(self):
        """Select previous line."""
        if self.selected_line > 0:
            self.selected_line -= 1
            if self.selected_line < self.scroll_position:
                self.scroll_up()
                
    def select_next(self):
        """Select next line."""
        if self.selected_line < len(self.content) - 1:
            self.selected_line += 1
            if self.selected_line >= self.scroll_position + self._viewable_height():
                self.scroll_down()
                
    def page_up(self):
        """Scroll up by one page."""
        self.scroll_up(self._viewable_height())
        
    def page_down(self):
        """Scroll down by one page."""
        self.scroll_down(self._viewable_height())
        
    def _viewable_height(self) -> int:
        """Get the viewable content height."""
        return self.height - (2 if self.border else 0)
        

class MenuWindow(ScrollableWindow):
    """Window for displaying menus."""
    
    def __init__(self, height: int, width: int, y: int, x: int,
                 title: Optional[str] = None,
                 items: Optional[List[Tuple[str, Callable]]] = None):
        super().__init__(height, width, y, x, title, True, Colors.MENU)
        self.items = items or []
        self.callbacks = {}
        self._setup_menu()
        
    def _setup_menu(self):
        """Setup menu items."""
        content = []
        for i, (label, callback) in enumerate(self.items):
            content.append(f"  {label}")
            self.callbacks[i] = callback
        self.set_content(content)
        
    def set_items(self, items: List[Tuple[str, Callable]]):
        """Set menu items."""
        self.items = items
        self.callbacks = {}
        self._setup_menu()
        
    def execute_selected(self):
        """Execute the callback for selected item."""
        if self.selected_line in self.callbacks:
            self.callbacks[self.selected_line]()
            
    def handle_key(self, key: int) -> bool:
        """Handle key press. Returns True if handled."""
        if key == Keys.UP:
            self.select_previous()
        elif key == Keys.DOWN:
            self.select_next()
        elif key == Keys.ENTER or key == Keys.SPACE:
            self.execute_selected()
        elif key == Keys.PAGE_UP:
            self.page_up()
        elif key == Keys.PAGE_DOWN:
            self.page_down()
        else:
            return False
        return True