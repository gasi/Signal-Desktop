// // TODO: How can we use CommonJS modules?
// import * as React from 'react';

// WORKAROUND: Use global `React` on `window`:
const React = window.React;
const {ConversationListItem} = window.Whisper.React;

interface Props {
  items: Array<Conversation>
  onItemSelect: ({item: Conversation}) => void
}

interface State {
  selectedItemId: string
}

// TODO: Extract once we have module bundling support:
interface Conversation {
  readonly id: string
  readonly active_at: number
  readonly avatar: Avatar
  readonly avatarUrl?: string
  readonly color?: null
  readonly expireTimer?: null
  readonly lastMessage: string
  readonly name: string
  readonly profileAvatar: ProfileAvatar
  readonly profileKey: object
  readonly profileName: string
  readonly profileSharing: boolean
  readonly timestamp: number
  readonly tokens?: Array<string> | null
  readonly type: string
  readonly unreadCount: number
  readonly verified: number

  // Backbone specific type signatures:
  readonly cid: string
  readonly get: (key: 'lastMessage' | 'name' | 'timestamp' | 'type') => any
}

interface ProfileAvatar {
  readonly data: object
  readonly contentType: string
  readonly size: number
}

interface Avatar {
  readonly data: object
  readonly contentType: string
  readonly length: number
}

class ConversationList extends React.PureComponent<Props, State> {
  constructor(props: Props) {
    super(props)

    this.state = {
      selectedItemId: null,
    }
  }

  handleItemClick = (event) => {
    const {id} = event
    this.setState({selectedItemId: id})

    const {items, onItemSelect} = this.props
    if (onItemSelect) {
      const selectedItem = items.find(item => item.cid === id)
      onItemSelect({item: selectedItem})
    }
  }

  render() {
    return (
      <div>
        {this.props.items.map(item =>
          <ConversationListItem
            key={`ConversationListItem-${item.cid}`}
            avatarURL={item.avatarUrl}
            id={item.cid}
            isSelected={this.state.selectedItemId === item.cid}
            lastMessage={item.get('lastMessage')}
            lastMessageTimestamp={item.get('timestamp')}
            name={item.get('name')}
            onClick={this.handleItemClick}
            type={item.get('type')}
          />
        )}
      </div>
    );
  }
}

// WORKAROUND: Expose React component on global `window.Whisper.React`:
window.Whisper.React.ConversationList = ConversationList;
