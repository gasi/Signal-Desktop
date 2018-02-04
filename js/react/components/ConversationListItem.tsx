// // TODO: How can we use CommonJS modules?
// import * as React from 'react';

// WORKAROUND: Use global `React` on `window`:
const React = window.React;
const classNames = window.classNames;

interface Props {
  // NOTE: Uses Backbone model collection ID (`cid`):
  id: string
  isSelected: boolean
  name: string
  lastMessage: string
  lastMessageTimestamp: number
}

class ConversationListItem extends React.PureComponent<Props, {}> {
  render() {
    return (
      <div
        className={
          classNames({
            'conversation-list-item': true,
            'contact': true,
            [this.props.id]: true,
            'selected': this.props.isSelected,
          })
        }
      >
        <span
          aria-hidden=""
          className="avatar"
          style={{
            backgroundImage: `url(${this.props.avatarURL})`
          }}
        />
        <div className="contact-details">
          <span
            className="last-timestamp"
            data-timestamp={this.props.lastMessageTimestamp}
            dir="auto"
          />
          <h3 className="name" dir="auto">{this.props.name}</h3>
          <div className="number">
            (314) 368-5827
          </div>
          <p className="last-message" dir="auto">
            {this.props.lastMessage}
          </p>
        </div>
      </div>
    );
  }
}

// WORKAROUND: Expose React component on global `window.Whisper.React`:
window.Whisper.React.ConversationListItem = ConversationListItem;
