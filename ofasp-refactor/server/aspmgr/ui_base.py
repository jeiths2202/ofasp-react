#!/usr/bin/env python3
"""
ASP Manager UI Base Components
Provides base classes for the user interface components
"""

import curses
import logging
from abc import ABC, abstractmethod
from typing import Optional, Tuple, List, Dict, Any
from dataclasses import dataclass
from enum import Enum

from .config import get_config, get_color_pairs, get_unicode_chars


class WindowType(Enum):
    """Types of windows in the interface"""
    MAIN = "main"
    DIALOG = "dialog"
    MENU = "menu"
    STATUS = "status"
    CONTENT = "content"


@dataclass
class Position:
    """Position coordinates"""
    y: int
    x: int


@dataclass
class Size:
    """Size dimensions"""
    height: int
    width: int


@dataclass
class Rect:
    """Rectangle with position and size"""
    position: Position
    size: Size
    
    @property
    def y(self) -> int:
        return self.position.y
    
    @property
    def x(self) -> int:
        return self.position.x
    
    @property
    def height(self) -> int:
        return self.size.height
    
    @property
    def width(self) -> int:
        return self.size.width
    
    @property
    def right(self) -> int:
        return self.x + self.width
    
    @property
    def bottom(self) -> int:
        return self.y + self.height


class BaseWindow(ABC):
    """Base class for all window components"""
    
    def __init__(self, parent_window: Optional[curses.window], rect: Rect, 
                 window_type: WindowType = WindowType.MAIN):
        self.parent_window = parent_window
        self.rect = rect
        self.window_type = window_type
        self.window: Optional[curses.window] = None
        self.is_visible = True
        self.is_focused = False
        self.config = get_config()
        self.color_pairs = get_color_pairs()
        self.unicode_chars = get_unicode_chars()
        self.logger = logging.getLogger(self.__class__.__name__)
        
        # Create the curses window
        self._create_window()
    
    def _create_window(self):
        """Create the curses window"""
        try:
            if self.parent_window:
                self.window = self.parent_window.subwin(
                    self.rect.height, self.rect.width,
                    self.rect.y, self.rect.x
                )
            else:
                self.window = curses.newwin(
                    self.rect.height, self.rect.width,
                    self.rect.y, self.rect.x
                )
            
            if self.window:
                self.window.keypad(True)
                self.window.nodelay(True)
                
        except curses.error as e:
            self.logger.error(f"Failed to create window: {e}")
            self.window = None
    
    def resize(self, rect: Rect):
        """Resize the window"""
        self.rect = rect
        if self.window:
            try:
                self.window.resize(rect.height, rect.width)
                self.window.mvwin(rect.y, rect.x)
            except curses.error as e:
                self.logger.error(f"Failed to resize window: {e}")
    
    def set_focus(self, focused: bool):
        """Set focus state"""
        self.is_focused = focused
        self.refresh()
    
    def show(self):
        """Show the window"""
        self.is_visible = True
        self.refresh()
    
    def hide(self):
        """Hide the window"""
        self.is_visible = False
        if self.window:
            self.window.clear()
            self.window.refresh()
    
    def clear(self):
        """Clear the window content"""
        if self.window:
            self.window.clear()
    
    def refresh(self):
        """Refresh the window display"""
        if self.window and self.is_visible:
            try:
                self.window.refresh()
            except curses.error:
                pass
    
    def get_key(self) -> int:
        """Get a key press from the window"""
        if self.window:
            try:
                return self.window.getch()
            except curses.error:
                return -1
        return -1
    
    def draw_border(self, style: str = 'normal'):
        """Draw border around the window"""
        if not self.window:
            return
        
        try:
            chars = self.unicode_chars
            color = self.color_pairs.get(style, self.color_pairs['normal'])
            
            # Set color
            if hasattr(curses, 'color_pair'):
                self.window.attrset(curses.color_pair(1))
            
            # Draw corners
            self.window.addch(0, 0, chars['tl_corner'])
            self.window.addch(0, self.rect.width - 1, chars['tr_corner'])
            self.window.addch(self.rect.height - 1, 0, chars['bl_corner'])
            self.window.addch(self.rect.height - 1, self.rect.width - 1, chars['br_corner'])
            
            # Draw horizontal lines
            for x in range(1, self.rect.width - 1):
                self.window.addch(0, x, chars['h_line'])
                self.window.addch(self.rect.height - 1, x, chars['h_line'])
            
            # Draw vertical lines
            for y in range(1, self.rect.height - 1):
                self.window.addch(y, 0, chars['v_line'])
                self.window.addch(y, self.rect.width - 1, chars['v_line'])
            
            # Reset attributes
            self.window.attrset(curses.A_NORMAL)
            
        except curses.error:
            pass
    
    def draw_text(self, y: int, x: int, text: str, style: str = 'normal', 
                  max_width: Optional[int] = None):
        """Draw text at specified position"""
        if not self.window:
            return
        
        try:
            # Truncate text if needed
            if max_width and len(text) > max_width:
                text = text[:max_width - 3] + '...'
            
            # Set color
            color = self.color_pairs.get(style, self.color_pairs['normal'])
            if hasattr(curses, 'color_pair'):
                self.window.attrset(curses.color_pair(1))
            
            # Draw text
            self.window.addstr(y, x, text)
            
            # Reset attributes
            self.window.attrset(curses.A_NORMAL)
            
        except curses.error:
            pass
    
    def draw_centered_text(self, y: int, text: str, style: str = 'normal'):
        """Draw centered text"""
        if not self.window:
            return
        
        x = max(0, (self.rect.width - len(text)) // 2)
        self.draw_text(y, x, text, style)
    
    def fill_line(self, y: int, char: str = ' ', style: str = 'normal'):
        """Fill a line with a character"""
        if not self.window:
            return
        
        try:
            line = char * self.rect.width
            self.draw_text(y, 0, line, style)
        except curses.error:
            pass
    
    def draw_progress_bar(self, y: int, x: int, width: int, percentage: float, 
                         style: str = 'normal'):
        """Draw a progress bar"""
        if not self.window:
            return
        
        try:
            filled_width = int(width * percentage / 100)
            empty_width = width - filled_width
            
            # Draw filled portion
            filled_bar = '█' * filled_width if self.config.terminal.use_unicode else '#' * filled_width
            self.draw_text(y, x, filled_bar, style)
            
            # Draw empty portion
            empty_bar = '░' * empty_width if self.config.terminal.use_unicode else '-' * empty_width
            self.draw_text(y, x + filled_width, empty_bar, 'border')
            
        except curses.error:
            pass
    
    @abstractmethod
    def draw(self):
        """Draw the window content - must be implemented by subclasses"""
        pass
    
    @abstractmethod
    def handle_input(self, key: int) -> bool:
        """Handle input - must be implemented by subclasses"""
        pass
    
    def cleanup(self):
        """Clean up resources"""
        if self.window:
            try:
                self.window.clear()
                self.window.refresh()
            except curses.error:
                pass


class ScrollableWindow(BaseWindow):
    """A window that supports scrolling content"""
    
    def __init__(self, parent_window: Optional[curses.window], rect: Rect, 
                 window_type: WindowType = WindowType.CONTENT):
        super().__init__(parent_window, rect, window_type)
        self.scroll_offset = 0
        self.content_lines: List[str] = []
        self.max_scroll = 0
    
    def set_content(self, lines: List[str]):
        """Set the content lines"""
        self.content_lines = lines
        self.max_scroll = max(0, len(lines) - (self.rect.height - 2))
        self.scroll_offset = min(self.scroll_offset, self.max_scroll)
    
    def scroll_up(self, lines: int = 1):
        """Scroll up by specified lines"""
        self.scroll_offset = max(0, self.scroll_offset - lines)
    
    def scroll_down(self, lines: int = 1):
        """Scroll down by specified lines"""
        self.scroll_offset = min(self.max_scroll, self.scroll_offset + lines)
    
    def scroll_to_top(self):
        """Scroll to the top"""
        self.scroll_offset = 0
    
    def scroll_to_bottom(self):
        """Scroll to the bottom"""
        self.scroll_offset = self.max_scroll
    
    def draw(self):
        """Draw the scrollable content"""
        if not self.window:
            return
        
        self.clear()
        self.draw_border()
        
        # Draw content
        visible_height = self.rect.height - 2
        start_line = self.scroll_offset
        end_line = min(start_line + visible_height, len(self.content_lines))
        
        for i, line_idx in enumerate(range(start_line, end_line)):
            if line_idx < len(self.content_lines):
                line = self.content_lines[line_idx]
                max_width = self.rect.width - 2
                self.draw_text(i + 1, 1, line, 'normal', max_width)
        
        # Draw scrollbar if needed
        if self.max_scroll > 0:
            self._draw_scrollbar()
    
    def _draw_scrollbar(self):
        """Draw a scrollbar on the right side"""
        if not self.window:
            return
        
        try:
            scrollbar_height = self.rect.height - 2
            scrollbar_pos = int(scrollbar_height * self.scroll_offset / self.max_scroll) if self.max_scroll > 0 else 0
            
            # Draw scrollbar track
            for y in range(1, self.rect.height - 1):
                char = '█' if y == scrollbar_pos + 1 else '░'
                if not self.config.terminal.use_unicode:
                    char = '#' if y == scrollbar_pos + 1 else '|'
                self.window.addch(y, self.rect.width - 1, char)
                
        except curses.error:
            pass
    
    def handle_input(self, key: int) -> bool:
        """Handle scrolling input"""
        if key == curses.KEY_UP:
            self.scroll_up()
            return True
        elif key == curses.KEY_DOWN:
            self.scroll_down()
            return True
        elif key == curses.KEY_PPAGE:
            self.scroll_up(self.rect.height - 2)
            return True
        elif key == curses.KEY_NPAGE:
            self.scroll_down(self.rect.height - 2)
            return True
        elif key == curses.KEY_HOME:
            self.scroll_to_top()
            return True
        elif key == curses.KEY_END:
            self.scroll_to_bottom()
            return True
        
        return False


class MenuWindow(BaseWindow):
    """A window that displays a menu with selectable items"""
    
    def __init__(self, parent_window: Optional[curses.window], rect: Rect, 
                 items: List[str], window_type: WindowType = WindowType.MENU):
        super().__init__(parent_window, rect, window_type)
        self.items = items
        self.selected_index = 0
        self.top_index = 0
    
    def set_items(self, items: List[str]):
        """Set the menu items"""
        self.items = items
        self.selected_index = min(self.selected_index, len(items) - 1)
        self._adjust_view()
    
    def get_selected_item(self) -> Optional[str]:
        """Get the currently selected item"""
        if 0 <= self.selected_index < len(self.items):
            return self.items[self.selected_index]
        return None
    
    def get_selected_index(self) -> int:
        """Get the currently selected index"""
        return self.selected_index
    
    def select_next(self):
        """Select the next item"""
        if self.selected_index < len(self.items) - 1:
            self.selected_index += 1
            self._adjust_view()
    
    def select_previous(self):
        """Select the previous item"""
        if self.selected_index > 0:
            self.selected_index -= 1
            self._adjust_view()
    
    def _adjust_view(self):
        """Adjust the view to show the selected item"""
        visible_height = self.rect.height - 2
        
        if self.selected_index < self.top_index:
            self.top_index = self.selected_index
        elif self.selected_index >= self.top_index + visible_height:
            self.top_index = self.selected_index - visible_height + 1
    
    def draw(self):
        """Draw the menu"""
        if not self.window:
            return
        
        self.clear()
        self.draw_border()
        
        visible_height = self.rect.height - 2
        end_index = min(self.top_index + visible_height, len(self.items))
        
        for i, item_index in enumerate(range(self.top_index, end_index)):
            if item_index < len(self.items):
                item = self.items[item_index]
                style = 'selected' if item_index == self.selected_index else 'normal'
                
                # Add selection indicator
                if item_index == self.selected_index:
                    indicator = self.unicode_chars['arrow_right']
                    self.draw_text(i + 1, 1, indicator, style)
                    self.draw_text(i + 1, 3, item, style, self.rect.width - 4)
                else:
                    self.draw_text(i + 1, 3, item, style, self.rect.width - 4)
    
    def handle_input(self, key: int) -> bool:
        """Handle menu navigation"""
        if key == curses.KEY_UP:
            self.select_previous()
            return True
        elif key == curses.KEY_DOWN:
            self.select_next()
            return True
        elif key == curses.KEY_HOME:
            self.selected_index = 0
            self._adjust_view()
            return True
        elif key == curses.KEY_END:
            self.selected_index = len(self.items) - 1
            self._adjust_view()
            return True
        
        return False