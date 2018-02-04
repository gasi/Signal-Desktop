// // TODO: How can we use CommonJS modules?
// import * as React from 'react';

const {hashCode} = window.Whisper.React;

// WORKAROUND: Use global `React` on `window`:
const React = window.React;
const classNames = window.classNames;

interface Props {
  // NOTE: Uses Backbone model collection ID (`cid`):
  id: string
  isSelected: boolean
  lastMessage: string
  lastMessageTimestamp: number
  name?: string
  type: 'private' | 'group'
}

interface AvatarProps {
  color: string
  content?: string
  url?: string
}

const COLORS = [
  'red',
  'pink',
  'purple',
  'deep_purple',
  'indigo',
  'blue',
  'light_blue',
  'cyan',
  'teal',
  'green',
  'light_green',
  'orange',
  'deep_orange',
  'amber',
  'blue_grey',
];

const Avatar = (props: AvatarProps) => {
  const hasURL = typeof props.url === 'string'
  const style = hasURL ?
    {backgroundImage: `url(${props.url})`} : null

  return (
    <span
      aria-hidden={}
      className={classNames({
        'avatar': true,
        [props.color]: !hasURL
      })}
      style={style}
    >
      {!hasURL ? props.content : null}
    </span>
  );
}

class ConversationListItem extends React.PureComponent<Props, {}> {
  getTitle() {
    const {id, name, type} = this.props

    switch (type) {
      case 'private':
        return (name || id || '').trim()

      case 'group':
        return (name || 'Unknown group').trim()

      default:
        throw new TypeError(
          `ConversationListItem::getTitle: Unknown \`type\`: ${type}`
        )
    }
  }

  getColor() {
    const title = this.getTitle()
    const normalizedHashCode = Math.abs(hashCode(title))
    return COLORS[normalizedHashCode % COLORS.length]
  }

  render() {
    const title = this.getTitle()
    const avatarContent = title[0] || '#'
    const avatarColor = this.getColor()

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
        <Avatar
          url={this.props.avatarURL}
          color={avatarColor}
          content={avatarContent}
        />
        <div className="contact-details">
          <span
            className="last-timestamp"
            data-timestamp={this.props.lastMessageTimestamp}
            dir="auto"
          />
          <h3 className="name" dir="auto">
            {this.props.name}
          </h3>
          <div className="number">
            {this.props.id}
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
