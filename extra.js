// ── supabaseR theme toggle: light / dark / auto ──
(function () {
  'use strict';

  var STORAGE_KEY = 'sb-theme';
  var ATTR = 'data-sb-theme';

  // SVG icons
  var ICON_SUN =
    '<svg viewBox="0 0 24 24"><path d="M12 7a5 5 0 100 10 5 5 0 000-10zm0-3a1 1 0 01-1-1V1a1 1 0 112 0v2a1 1 0 01-1 1zm0 18a1 1 0 01-1-1v-2a1 1 0 112 0v2a1 1 0 01-1 1zm9-9a1 1 0 01-1 1h-2a1 1 0 110-2h2a1 1 0 011 1zM5 12a1 1 0 01-1 1H2a1 1 0 110-2h2a1 1 0 011 1zm14.07-6.36a1 1 0 010 1.41l-1.41 1.42a1 1 0 11-1.42-1.42l1.42-1.41a1 1 0 011.41 0zM7.76 17.66a1 1 0 010 1.41l-1.41 1.42a1 1 0 11-1.42-1.42l1.42-1.41a1 1 0 011.41 0zm10.48 1.42a1 1 0 01-1.41 0l-1.42-1.41a1 1 0 111.42-1.42l1.41 1.42a1 1 0 010 1.41zM6.34 7.76a1 1 0 01-1.41 0L3.51 6.35a1 1 0 011.42-1.42l1.41 1.42a1 1 0 010 1.41z"/></svg>';
  var ICON_MOON =
    '<svg viewBox="0 0 24 24"><path d="M21.64 13a1 1 0 00-1.05-.14 8.05 8.05 0 01-3.37.73A8.15 8.15 0 019.08 5.49a8.59 8.59 0 01.25-2 1 1 0 00-.64-1.11 1 1 0 00-1.27.52 10.14 10.14 0 1014.22 10.1z"/></svg>';
  var ICON_AUTO =
    '<svg viewBox="0 0 24 24"><path d="M12 22C6.477 22 2 17.523 2 12S6.477 2 12 2s10 4.477 10 10-4.477 10-10 10zm0-2V4a8 8 0 100 16z"/></svg>';

  function getSystemTheme() {
    return window.matchMedia('(prefers-color-scheme: dark)').matches
      ? 'dark'
      : 'light';
  }

  function applyTheme(pref) {
    var resolved = pref === 'auto' ? getSystemTheme() : pref;
    document.documentElement.setAttribute(ATTR, resolved);
    // also set BS5 attribute so pkgdown components follow
    document.documentElement.setAttribute('data-bs-theme', resolved);
  }

  function getSaved() {
    try { return localStorage.getItem(STORAGE_KEY) || 'dark'; }
    catch (e) { return 'dark'; }
  }

  function save(pref) {
    try { localStorage.setItem(STORAGE_KEY, pref); } catch (e) {}
  }

  // Apply early (before paint) – this runs in <body> so it's quick
  var current = getSaved();
  applyTheme(current);

  // Listen for system changes when in auto mode
  window.matchMedia('(prefers-color-scheme: dark)').addEventListener('change', function () {
    if (getSaved() === 'auto') applyTheme('auto');
  });

  function buildToggle() {
    var wrap = document.createElement('div');
    wrap.className = 'sb-theme-toggle';
    wrap.setAttribute('role', 'radiogroup');
    wrap.setAttribute('aria-label', 'Theme');

    var modes = [
      { id: 'light', icon: ICON_SUN, label: 'Light' },
      { id: 'dark', icon: ICON_MOON, label: 'Dark' },
      { id: 'auto', icon: ICON_AUTO, label: 'System' }
    ];

    modes.forEach(function (m) {
      var btn = document.createElement('button');
      btn.setAttribute('role', 'radio');
      btn.setAttribute('aria-label', m.label);
      btn.setAttribute('title', m.label);
      btn.setAttribute('data-theme-value', m.id);
      btn.innerHTML = m.icon;
      if (m.id === current) btn.classList.add('active');
      btn.addEventListener('click', function () {
        current = m.id;
        save(current);
        applyTheme(current);
        wrap.querySelectorAll('button').forEach(function (b) {
          b.classList.toggle('active', b.getAttribute('data-theme-value') === current);
          b.setAttribute('aria-checked', b.getAttribute('data-theme-value') === current);
        });
      });
      wrap.appendChild(btn);
    });

    return wrap;
  }

  // Insert toggle into navbar once DOM is ready
  function insertToggle() {
    // Try the right-side nav area
    var navbar = document.querySelector('.navbar-nav.navbar-nav-icons') ||
                 document.querySelector('.navbar .navbar-right') ||
                 document.querySelector('.navbar-nav:last-child');
    if (navbar) {
      var li = document.createElement('li');
      li.className = 'nav-item d-flex align-items-center';
      li.appendChild(buildToggle());
      navbar.appendChild(li);
    } else {
      // Fallback: append to the navbar container
      var nav = document.querySelector('.navbar > .container') ||
                document.querySelector('.navbar > .container-fluid') ||
                document.querySelector('.navbar');
      if (nav) {
        var div = document.createElement('div');
        div.className = 'd-flex align-items-center ms-auto';
        div.appendChild(buildToggle());
        nav.appendChild(div);
      }
    }
  }

  if (document.readyState === 'loading') {
    document.addEventListener('DOMContentLoaded', insertToggle);
  } else {
    insertToggle();
  }
})();
